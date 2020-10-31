{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}

module Kathu.World.WorldSpace where

import           Apecs                     hiding (Map)
import qualified Apecs
import           Apecs.Physics             hiding (Map)
import           Control.Lens              hiding ((.=), set)
import           Control.Monad             (foldM, forM_, when)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Aeson
import           Data.Aeson.Types          (Parser, typeMismatch)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vec
import           Verda.Graphics.Sprites    (Sprite)
import           Verda.IO.Directory        (WorkingDirectory)
import           Verda.Parsing.Yaml        (FieldOrder, mkFieldOrderFromList)
import           Verda.System.Tile.Chunks  (unitsPerTile)
import           Verda.System.Tile.Components
import           Verda.Util.Apecs
import           Verda.Util.Dependency
import           Verda.Util.Types          (Identifier, IDMap)
import           Verda.World               (Existance(..))

import           Kathu.Entity.Components
import           Kathu.Entity.Item
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Physics.CollisionGroup
import           Kathu.Entity.Prefab       (Prefab, prefabID)
import           Kathu.Entity.System       (IncludeEditorInfo(..))
import           Kathu.Graphics.Palette
import           Kathu.Scripting.Event
import qualified Kathu.Scripting.Lua       as Lua
import           Kathu.Scripting.Variables
import           Kathu.Scripting.Wire
import           Kathu.World.ChunkBuilder
import           Kathu.World.Stasis
import           Kathu.World.Tile          (AllTiles', Tile)

data InstancedPrefab = InstancedPrefab
    { _instanceID         :: !Identifier
    , _basePrefab         :: Prefab
    , _spawnLocation      :: !(V2 Double)
    , _wireSignalEmitter  :: !(Maybe Identifier)
    , _wireSignalReceiver :: !(Maybe Identifier)
    , _instanceConfig     :: !(IDMap WorldVariable)
    }
makeLenses ''InstancedPrefab

-- | Marks an entity as having been created by an instance prototype specified by the loaded Worldspace; the Int is for the prototype's index
newtype EditorInstancedFromWorld = EditorInstancedFromWorld {unEditorInstancedFromWorld :: InstancedPrefab}
-- Should be just a map, and is expected to be entirely empty when running the game normally
instance Component EditorInstancedFromWorld where type Storage EditorInstancedFromWorld = Apecs.Map EditorInstancedFromWorld

emptyInstancedPrefab :: InstancedPrefab
emptyInstancedPrefab = InstancedPrefab "" prototype (V2 0 0) Nothing Nothing Map.empty
    where prototype = error "Attempted to use emptyInstancedPrefab basePrefab; no possible value exists for this"

data InstancedItem = InstancedItem
    { _baseItem     :: ItemStack
    , _itemPosition :: !(V2 Double)
    }
makeLenses ''InstancedItem

data WorldSpace = WorldSpace
    { _worldID            :: !Identifier
    , _worldName          :: !Text
    , _initialPalette     :: !Identifier
    , _worldPalettes      :: !(IDMap Palette)
    , _loadPoint          :: !(V2 Double)
    , _shouldSavePosition :: !Bool -- when serialized, should we remember where the player was?
    , _worldVariables     :: !(IDMap WorldVariable)
    , _worldScript        :: !(Maybe Lua.Script)
    , _worldEntities      :: !(Vector InstancedPrefab)
    , _worldItems         :: !(Vector InstancedItem)
    , _worldChunks        :: !Chunks
    }
makeLenses ''WorldSpace

instance Semigroup WorldSpace  where (<>) = mappend
instance Monoid WorldSpace  where mempty = emptyWorldSpace
instance Component WorldSpace  where type Storage WorldSpace  = Global WorldSpace

emptyWorldSpace :: WorldSpace
emptyWorldSpace = WorldSpace "" "the void..." "" Map.empty (V2 0 0) False Map.empty Nothing Vec.empty Vec.empty emptyChunks

--------------------
-- System Related --
--------------------

initWorldSpace :: forall w. (Get w IO EntityCounter, Get w IO AllTiles', Has w IO Physics, Lua.HasScripting w IO, ReadWriteEach w IO '[Chunks, EditorInstancedFromWorld, Existance, IncludeEditorInfo, LifeTime, Player, SpecialEntity, Sprite, Variables, WireReceivers, WorldSpace, WorldStases])
               => (Entity -> SystemT w IO ())
               -> ((Lua.ActiveScript -> Lua.ActiveScript) -> Prefab -> SystemT w IO Entity)
               -> (Entity -> Lua.Script -> SystemT w IO Lua.ActiveScript)
               -> WorldSpace
               -> SystemT w IO ()
initWorldSpace destroyEty mkEntity loadScript ws = do
    -- we clean up all previous entities without lifetimes
    cmapM_ $ \(Existance, _ :: Not LifeTime, ety) -> destroyEty ety
    -- for those that have them, we clear those that are not persistant
    cmapM_ $ \(lf :: LifeTime, ety)    -> when (lf /= Persistant) $ destroyEty ety
    
    saveWorldVariables ws

    global $= ws

    includeEditorInfo <- get global

    cmap $ \(_ :: Player) -> ws^.loadPoint.to Position
    -- we place all items in the world as entities
    forM_ (ws^.worldItems) $ \(InstancedItem item pos) -> newEntityFromItem item pos
    forM_ (ws^.worldEntities) $ placeInstancedPrefab includeEditorInfo mkEntity

    global $= ws^.worldChunks
    buildTileCollisions ws

    case ws^.worldScript of
        Nothing    -> pure ()
        (Just scr) -> do
            ety    <- newExistingEntity ()
            active <- loadScript ety scr
            ety    $= active

placeInstancedPrefab :: forall w. (Get w IO EntityCounter, Has w IO Physics, Lua.HasScripting w IO, ReadWriteEach w IO '[EditorInstancedFromWorld, Existance, SpecialEntity, Sprite, WireReceivers])
                     => IncludeEditorInfo
                     -> ((Lua.ActiveScript -> Lua.ActiveScript) -> Prefab -> SystemT w IO Entity)
                     -> InstancedPrefab
                     -> SystemT w IO ()
placeInstancedPrefab (IncludeEditorInfo shouldIncludeInfo) mkEntity instancedPrefab@(InstancedPrefab _ prefab pos sigOut sigIn config) = do
    ety <- mkEntity (\s -> s {Lua.instanceConfig = config}) prefab
    ety $= Position pos
    -- include the instance with the entity, as we are running in some form of editor mode
    when shouldIncludeInfo $
        ety $= EditorInstancedFromWorld instancedPrefab

    initialScript <- getIfExists ety
    forM_ initialScript $
        set ety . Lua.setInstanceConfig config 

    forM_ sigOut $ \sig -> modify ety (Lua.addWireController sig)

    forM_ sigIn $ \sig -> do
        maybeScript <- getIfExists ety
        forM_ maybeScript $ \script ->
                when (Lua.shouldScriptRun onSignalChange script) $ Lua.addWireReceiver sig script

-- | Destroy all entities created for holding tile collisions
destroyTileCollisions :: forall w. (ReadWrite w IO SpecialEntity) => (Entity -> SystemT w IO ()) -> SystemT w IO ()
destroyTileCollisions destroyEty =
    cmapM_ $ \(specialEty, ety) -> when (specialEty == WorldCollision) $ destroyEty ety

buildTileCollisions :: forall w. (Get w IO EntityCounter, Get w IO AllTiles', Has w IO Physics, ReadWriteEach w IO '[Existance, SpecialEntity])
                      => WorldSpace
                      -> SystemT w IO ()
buildTileCollisions ws = do
    tiles :: AllTiles' <- get global
    let worldCollisionFilter = groupCollisionFilter Movement
        addWorldCollision polygons
            | Vec.null polygons = pure ()
            | otherwise         = do
                ety <- newExistingEntity (WorldCollision, StaticBody, Position (V2 0 0))
                ety $= (Shape ety (Convex (Vec.head polygons) 0), worldCollisionFilter)
                
                mapM_ (\p -> newExistingEntity (WorldCollision, Shape ety $ Convex p 0)) . Vec.tail $ polygons
    colPolys <- mkCollisionPolygons tiles $ ws^.worldChunks

    addWorldCollision colPolys

-- | Loads in the new variables for the current world, and saves the previous to its Stasis
saveWorldVariables :: forall w m. (MonadIO m, ReadWriteEach w m '[Variables, WorldSpace, WorldStases]) => WorldSpace -> SystemT w m ()
saveWorldVariables newWS = do
    oldWS :: WorldSpace <- get global
    variables <- get global
    stases    <- get global

    let oldID = oldWS^.worldID

    -- if we already have this world saved, we get its saved variables; otherwise we use the default ones
    let newVars = maybe (newWS^.worldVariables) statisVariables . getStasis oldID $ stases

    prevWorldVars <- replaceWorldVariables variables newVars

    let saveVar stasis = stasis {statisVariables = prevWorldVars}

    global $= updateStasis oldID stases saveVar

-- as of right now, count not considered; this will be added when picking up is implemented
-- currently use StaticBody, although DynamicBody will be used once these have a shape and mass
newEntityFromItem :: forall w m. (MonadIO m, Get w m EntityCounter, ReadWriteEach w m '[Body, Existance, Position, Sprite]) => ItemStack -> V2 Double -> SystemT w m Entity
newEntityFromItem stack v = newExistingEntity (StaticBody, Position v, itemIcon . stackItem $ stack)

-------------------
-- Serialization --
-------------------

instance ToJSON InstancedPrefab where
    toJSON (InstancedPrefab idt ety pos sigEmit sigRece config) = object
        [ "entity"           .= prefabID ety
        , "instance-id"      .= (if idt == "" then Nothing else Just idt)
        , "position"         .= (pos / unitsPerTile)
        , "emitting-signal"  .= sigEmit
        , "receiving-signal" .= sigRece
        , "config"           .= (if Map.null config then Nothing else Just config)
        ]

instance (s `CanProvide` (IDMap Prefab), Monad m) => FromJSON (Dependency s m InstancedPrefab) where
    parseJSON (Object obj) = do
        idt     <- obj .:? "instance-id" .!= ""
        pos     <- (*unitsPerTile) <$> obj .: "position"
        sigEmit <- obj .:? "emitting-signal"
        sigRece <- obj .:? "receiving-signal"
        config  <- obj .:? "config" .!= Map.empty
        entity  <- dependencyMapLookupElseError "Entity" <$> (obj .: "entity" :: Parser Identifier)
        pure $ (\e -> InstancedPrefab idt e pos sigEmit sigRece config) <$> entity
    parseJSON e = typeMismatch "InstancedPrefab" e

instance ToJSON InstancedItem where
    toJSON (InstancedItem (ItemStack item count) pos) = object
        [ "item"     .= itemID item
        , "count"    .= (if count == 1 then Nothing else Just count)
        , "position" .= (pos / unitsPerTile)
        ]

instance (FromJSON (Dependency s m ItemStack), Functor m) => FromJSON (Dependency s m InstancedItem) where
    parseJSON val@(Object obj) = do
        pos     <- (*unitsPerTile) <$> obj .: "position"
        stack   <- parseJSON val
        pure $ flip InstancedItem pos <$> stack
    parseJSON e = typeMismatch "InstancedItem" e

----------------
-- WorldSpace --
----------------

encodeValueForWorldSpace :: MonadIO m => AllTiles' -> WorldSpace -> m Value
encodeValueForWorldSpace allTiles (WorldSpace wid wname initPal palettes loadPos shouldSavePos vars script etys items chunks) = do
    fieldPairs <- serializeFieldSetPairs allTiles chunks
    pure . object $
        [ "world-id"                   .= wid
        , "name"                       .= wname
        , "script"                     .= script
        , "initial-palette"            .= initPal
        , "palettes"                   .= Map.keys palettes
        , "load-point"                 .= (loadPos / unitsPerTile)
        , "should-save-exact-position" .= shouldSavePos
        , "variables"                  .= vars
        , "entities"                   .= Vec.reverse etys  -- as part of loading this get reversed, so we un-reverse it once we save to ensure the same order
        , "items"                      .= Vec.reverse items
        ]
        ++ fieldPairs

instance ( s `CanProvide` IDMap Prefab
         , s `CanProvide` IDMap Palette
         , s `CanProvide` IDMap Tile
         , s `CanProvide` WorkingDirectory
         , FromJSON (Dependency s m ItemStack)
         , MonadIO m
         ) => FromJSON (Dependency s m WorldSpace) where
    parseJSON (Object v) = do
        worldId    <- v .: "world-id"
        wName      <- v .: "name"
        initPal    <- v .: "initial-palette"
        paletteIDs <- v .: "palettes"
        loadPnt    <- (*unitsPerTile) <$> v .: "load-point"
        variables  <- v .:? "variables" .!= Map.empty

        shouldSerializePosition <- v .:? "should-save-exact-position" .!= False
        chunkConfigs :: Vector ChunkConfig <- v .: "chunks"
        
        wScript :: Dependency s m (Maybe Lua.Script) <- flattenDependency <$> v .:? "script"

        dLegend :: Dependency s m MapLegend <- do
            (keys, vals) :: ([Text], [Identifier]) <- unzip . Map.toList <$> v .: "legend"

            pure . fmap (Map.fromList . zip (T.head <$> keys))
                 . flattenDependency
                 . fmap (dependencyMapLookupElseError "Tile")
                 $ vals

        items    <- v .: "items"
        entities <- v .: "entities"

        let palettes = dependencyMapLookupMap (paletteIDs :: Vector Identifier)

            newChunks :: Dependency s m Chunks
            newChunks = do
                legend <- dLegend
                liftDependency $
                    foldM (applyChunkConfig legend) emptyChunks chunkConfigs

        pure $ WorldSpace worldId wName initPal <$> palettes <*> pure loadPnt <*> pure shouldSerializePosition <*> pure variables <*> wScript <*> entities <*> items <*> newChunks
    parseJSON e          = typeMismatch "WorldSpace" e

-- | When we serialize the WorldSpace, we serialize the fields in the following order
worldspaceFieldOrder :: FieldOrder
worldspaceFieldOrder = mkFieldOrderFromList
    [ "world-id"
    , "name"
    , "script"
    , "initial-palette"
    , "palettes"
    , "load-point"
    , "should-save-exact-position"
    , "variables"
    , "legend"
    , "chunks"
    , "entities"
    , "items"
    -- Sub Configs
    , "entity"
    , "item"
    , "position"
    , "data"
    , "emitting-signal"
    , "receiving-signal"
    , "config"
    , "file"
    , "events"
    , "is-singleton"
    , "type"
    , "value"
    , " "
    ]