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
import           Data.Maybe                (catMaybes)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vec

import           Kathu.Entity.Components
import           Kathu.Entity.Item
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Physics.CollisionGroup
import           Kathu.Entity.Prototype    (EntityPrototype, getPrototypeID)
import           Kathu.Entity.System       (IncludeEditorInfo(..), Tiles)
import           Kathu.Graphics.Drawable   (Render)
import           Kathu.Graphics.Palette
import           Verda.IO.Directory        (WorkingDirectory)
import           Verda.Parsing.Yaml        (FieldOrder, mkFieldOrderFromList)
import           Kathu.Scripting.Event
import qualified Kathu.Scripting.Lua       as Lua
import           Kathu.Scripting.Variables
import           Kathu.Scripting.Wire
import           Verda.Util.Dependency
import           Verda.Util.Types          (Identifier, IDMap)
import           Kathu.World.Field
import           Kathu.World.Stasis
import           Kathu.World.Tile          (Tile)
import           Verda.Util.Apecs

data InstancedPrototype g = InstancedPrototype
    { _instanceID         :: !Identifier
    , _basePrototype      :: EntityPrototype g
    , _spawnLocation      :: !(V2 Double)
    , _wireSignalEmitter  :: !(Maybe Identifier)
    , _wireSignalReceiver :: !(Maybe Identifier)
    , _instanceConfig     :: !(IDMap WorldVariable)
    }
makeLenses ''InstancedPrototype

-- | Marks an entity as having been created by an instance prototype specified by the loaded Worldspace; the Int is for the prototype's index
newtype EditorInstancedFromWorld g = EditorInstancedFromWorld {unEditorInstancedFromWorld :: InstancedPrototype g}
-- Should be just a map, and is expected to be entirely empty when running the game normally
instance Component (EditorInstancedFromWorld g) where type Storage (EditorInstancedFromWorld g) = Apecs.Map (EditorInstancedFromWorld g)

emptyInstancedPrototype :: InstancedPrototype g
emptyInstancedPrototype = InstancedPrototype "" prototype (V2 0 0) Nothing Nothing Map.empty
    where prototype = error "Attempted to use emptyInstancedPrototype basePrototype; no possible value exists for this"

data InstancedItem g = InstancedItem
    { _baseItem     :: ItemStack g
    , _itemPosition :: !(V2 Double)
    }
makeLenses ''InstancedItem

data WorldSpace g = WorldSpace
    { _worldID            :: !Identifier
    , _worldName          :: !Text
    , _initialPalette     :: !Identifier
    , _worldPalettes      :: !(IDMap Palette)
    , _loadPoint          :: !(V2 Double)
    , _shouldSavePosition :: !Bool -- when serialized, should we remember where the player was?
    , _worldVariables     :: IDMap WorldVariable
    , _worldScript        :: !(Maybe Lua.Script)
    , _worldEntities      :: Vector (InstancedPrototype g)
    , _worldItems         :: Vector (InstancedItem g)
    , _worldFields        :: !FieldSet
    }
makeLenses ''WorldSpace

emptyWorldSpace :: WorldSpace g
emptyWorldSpace = WorldSpace "" "the void..." "" Map.empty (V2 0 0) False Map.empty Nothing Vec.empty Vec.empty emptyFieldSet

-- right now we only consider horizontal fields; ones with different z depths are ignored
fieldsSurrounding :: RealFrac a => a -> a -> WorldSpace g -> [(V2 Int, Field)]
fieldsSurrounding wx wy ws = catMaybes $ readFields [] (ox - 1) (oy - 1)
    where fields    = unFieldSet $ ws^.worldFields
          (# ox, oy #) = fieldContainingCoord wx wy
          readFields !acc !x !y | y > oy + 1 = acc
                                | x > ox + 1 = readFields acc (ox - 1) (y + 1)
                                | otherwise  = readFields (((curV,) <$> Map.lookup curV fields):acc) (x + 1) y
              where curV = V2 x y -- only consider same z level right now

-- | Returned the given Field, creating a new one and inserting into the current worldspace if the given field is missing
mkFieldIfNotPresent :: forall w g. (ReadWrite w IO (WorldSpace g)) => Proxy g -> V2 Int -> SystemT w IO Field
mkFieldIfNotPresent _ fieldPos = do
    ws :: WorldSpace g <- get global

    let fieldMap = ws^.worldFields.to unFieldSet

    case Map.lookup fieldPos fieldMap of
        Just field -> pure field
        Nothing    -> do
            field <- mkField
            let fieldMap' = FieldSet $ Map.insert fieldPos field fieldMap
            global $= (worldFields .~ fieldMap' $ ws)
            pure field

--------------------
-- System Related --
--------------------

initWorldSpace :: forall w g. (Get w IO EntityCounter, Get w IO (Tiles g), Has w IO Physics, Lua.HasScripting w IO, ReadWriteEach w IO '[EditorInstancedFromWorld g, Existance, IncludeEditorInfo, LifeTime, Player, Render g, SpecialEntity, Variables, WireReceivers, WorldSpace g, WorldStases])
               => (Entity -> SystemT w IO ())
               -> ((Lua.ActiveScript -> Lua.ActiveScript) -> EntityPrototype g -> SystemT w IO Entity)
               -> (Entity -> Lua.Script -> SystemT w IO Lua.ActiveScript)
               -> WorldSpace g
               -> SystemT w IO ()
initWorldSpace destroyEty mkEntity loadScript ws = do
    -- we clean up all previous entities without lifetimes
    cmapM_ $ \(Existance, _ :: Not LifeTime, ety) -> destroyEty ety
    -- for those that have them, we clear those that are not persistant
    cmapM_ $ \(lf :: LifeTime, ety)    -> when (lf /= Persistant) $ destroyEty ety
    
    saveWorldVariables ws

    global $= ws

    shouldIncludeInfo <- get global

    cmap $ \(_ :: Player) -> ws^.loadPoint.to Position
    -- we place all items in the world as entities
    forM_ (ws^.worldItems) $ \(InstancedItem item pos) -> newEntityFromItem item pos
    forM_ (ws^.worldEntities) $ placeInstancedPrototype shouldIncludeInfo mkEntity

    buildTileCollisions ws

    case ws^.worldScript of
        Nothing    -> pure ()
        (Just scr) -> do
            ety    <- newExistingEntity ()
            active <- loadScript ety scr
            ety    $= active

placeInstancedPrototype :: forall w g. (Get w IO EntityCounter, Has w IO Physics, Lua.HasScripting w IO, ReadWriteEach w IO '[EditorInstancedFromWorld g, Existance, Render g, SpecialEntity, WireReceivers])
                        => IncludeEditorInfo
                        -> ((Lua.ActiveScript -> Lua.ActiveScript) -> EntityPrototype g -> SystemT w IO Entity)
                        -> InstancedPrototype g
                        -> SystemT w IO ()
placeInstancedPrototype (IncludeEditorInfo shouldIncludeInfo) mkEntity instancedProto@(InstancedPrototype _ proto pos sigOut sigIn config) = do
    ety <- mkEntity (\s -> s {Lua.instanceConfig = config}) proto
    ety $= Position pos
    -- include the instance with the entity, as we are running in some form of editor mode
    when shouldIncludeInfo $
        ety $= EditorInstancedFromWorld instancedProto

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

buildTileCollisions :: forall w g. (Get w IO EntityCounter, Get w IO (Tiles g), Has w IO Physics, ReadWriteEach w IO '[Existance, SpecialEntity])
                      => WorldSpace g
                      -> SystemT w IO ()
buildTileCollisions ws = do
    tiles :: Tiles g <- get global
    let worldCollisionFilter = groupCollisionFilter Movement
        addWorldCollision polygons
            | Vec.null polygons = pure ()
            | otherwise         = do
                ety <- newExistingEntity (WorldCollision, StaticBody, Position (V2 0 0))
                ety $= (Shape ety (Convex (Vec.head polygons) 0), worldCollisionFilter)
                
                mapM_ (\p -> newExistingEntity (WorldCollision, Shape ety $ Convex p 0)) . Vec.tail $ polygons
    colPolys <- mkCollisionPolygons tiles $ ws^.worldFields

    addWorldCollision colPolys

-- | Loads in the new variables for the current world, and saves the previous to its Stasis
saveWorldVariables :: forall w m g. (MonadIO m, ReadWriteEach w m '[Variables, WorldSpace g, WorldStases]) => WorldSpace g -> SystemT w m ()
saveWorldVariables newWS = do
    oldWS :: WorldSpace g <- get global
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
newEntityFromItem :: forall w m g. (MonadIO m, Get w m EntityCounter, ReadWriteEach w m '[Body, Existance, Position, Render g]) => ItemStack g -> V2 Double -> SystemT w m Entity
newEntityFromItem stack v = newExistingEntity (StaticBody, Position v, itemIcon . stackItem $ stack)

-------------------
-- Serialization --
-------------------

instance ToJSON (InstancedPrototype g) where
    toJSON (InstancedPrototype idt ety pos sigEmit sigRece config) = object
        [ "entity"           .= getPrototypeID ety
        , "instance-id"      .= (if idt == "" then Nothing else Just idt)
        , "position"         .= (pos / unitsPerTile)
        , "emitting-signal"  .= sigEmit
        , "receiving-signal" .= sigRece
        , "config"           .= (if Map.null config then Nothing else Just config)
        ]

instance (s `CanProvide` (IDMap (EntityPrototype g)), Monad m) => FromJSON (Dependency s m (InstancedPrototype g)) where
    parseJSON (Object obj) = do
        idt     <- obj .:? "instance-id" .!= ""
        pos     <- (*unitsPerTile) <$> obj .: "position"
        sigEmit <- obj .:? "emitting-signal"
        sigRece <- obj .:? "receiving-signal"
        config  <- obj .:? "config" .!= Map.empty
        entity  <- dependencyMapLookupElseError "Entity" <$> (obj .: "entity" :: Parser Identifier)
        pure $ (\e -> InstancedPrototype idt e pos sigEmit sigRece config) <$> entity
    parseJSON e = typeMismatch "InstancedPrototype" e

instance ToJSON (InstancedItem g) where
    toJSON (InstancedItem (ItemStack item count) pos) = object
        [ "item"     .= itemID item
        , "count"    .= (if count == 1 then Nothing else Just count)
        , "position" .= (pos / unitsPerTile)
        ]

instance (FromJSON (Dependency s m (ItemStack g)), Functor m) => FromJSON (Dependency s m (InstancedItem g)) where
    parseJSON val@(Object obj) = do
        pos     <- (*unitsPerTile) <$> obj .: "position"
        stack   <- parseJSON val
        pure $ flip InstancedItem pos <$> stack
    parseJSON e = typeMismatch "InstancedItem" e

----------------
-- WorldSpace --
----------------

encodeValueForWorldSpace :: MonadIO m => Tiles g -> WorldSpace g -> m Value
encodeValueForWorldSpace allTiles (WorldSpace wid wname initPal palettes loadPos shouldSavePos vars script etys items fields) = do
    fieldPairs <- serializeFieldSetPairs allTiles fields
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

instance ( s `CanProvide` IDMap (EntityPrototype g)
         , s `CanProvide` IDMap Palette
         , s `CanProvide` IDMap (Tile g)
         , s `CanProvide` WorkingDirectory
         , FromJSON (Dependency s m (ItemStack g))
         , MonadIO m
         ) => FromJSON (Dependency s m (WorldSpace g)) where
    parseJSON (Object v) = do
        worldId    <- v .: "world-id"
        wName      <- v .: "name"
        initPal    <- v .: "initial-palette"
        paletteIDs <- v .: "palettes"
        loadPnt    <- (*unitsPerTile) <$> v .: "load-point"
        variables  <- v .:? "variables" .!= Map.empty

        shouldSerializePosition <- v .:? "should-save-exact-position" .!= False
        fieldConfigs :: Vector FieldConfig <- v .: "fields"
        
        wScript :: Dependency s m (Maybe Lua.Script) <- flattenDependency <$> v .:? "script"

        dLegend :: Dependency s m (MapLegend g) <- do
            (keys, vals) :: ([Text], [Identifier]) <- unzip . Map.toList <$> v .: "legend"

            pure . fmap (Map.fromList . zip (T.head <$> keys))
                 . flattenDependency
                 . fmap (dependencyMapLookupElseError "Tile")
                 $ vals

        items    <- v .: "items"
        entities <- v .: "entities"

        let palettes = dependencyMapLookupMap (paletteIDs :: Vector Identifier)

            newFieldSet :: Dependency s m FieldSet
            newFieldSet = do
                legend <- dLegend
                liftDependency $
                    foldM (applyFieldConfig legend) emptyFieldSet fieldConfigs

        pure $ WorldSpace worldId wName initPal <$> palettes <*> pure loadPnt <*> pure shouldSerializePosition <*> pure variables <*> wScript <*> entities <*> items <*> newFieldSet
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
    , "fields"
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