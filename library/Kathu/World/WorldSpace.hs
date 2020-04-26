{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}

module Kathu.World.WorldSpace where

import           Apecs                     hiding (Map)
import           Apecs.Physics             hiding (Map)
import           Control.Lens              hiding ((.=), set)
import           Control.Monad             (forM_, when)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Aeson
import           Data.Aeson.Types          (Parser, typeMismatch)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (catMaybes, fromMaybe, maybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vec
import           Linear.V2                 (V2(..))

import           Kathu.Entity.Components
import           Kathu.Entity.Item
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Physics.CollisionGroup
import           Kathu.Entity.Prototype    (EntityPrototype, getPrototypeID)
import           Kathu.Entity.System       (Tiles)
import           Kathu.Graphics.Drawable   (Render)
import           Kathu.Graphics.Palette
import           Kathu.IO.Directory        (WorkingDirectory)
import           Kathu.Scripting.Event
import qualified Kathu.Scripting.Lua       as Lua
import           Kathu.Scripting.Variables
import           Kathu.Scripting.Wire
import           Kathu.Util.Apecs
import           Kathu.Util.Dependency
import           Kathu.Util.Types          (Identifier, IDMap)
import           Kathu.World.Field
import           Kathu.World.Stasis
import           Kathu.World.Tile          (Tile, _tileTextID)

data InstancedPrototype g = InstancedPrototype
    { _instanceID         :: !Identifier
    , _basePrototype      :: EntityPrototype g
    , _spawnLocation      :: !(V2 Double)
    , _wireSignalEmitter  :: !(Maybe Identifier)
    , _wireSignalReceiver :: !(Maybe Identifier)
    , _instanceConfig     :: !(IDMap WorldVariable)
    }
makeLenses ''InstancedPrototype

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
    , _worldLegend        :: !(Map Char (Tile g))
    }
makeLenses ''WorldSpace

emptyWorldSpace :: WorldSpace g
emptyWorldSpace = WorldSpace "" "the void..." "" Map.empty (V2 0 0) False Map.empty Nothing Vec.empty Vec.empty emptyFieldSet Map.empty

-- right now we only consider horizontal fields; ones with different z depths are ignored
fieldsSurrounding :: RealFrac a => a -> a -> WorldSpace g -> [(V2 Int, Field)]
fieldsSurrounding wx wy ws = catMaybes $ readFields [] (ox - 1) (oy - 1)
    where fields    = unFieldSet $ ws^.worldFields
          (# ox, oy #) = fieldContainingCoord wx wy
          readFields !acc !x !y | y > oy + 1 = acc
                                | x > ox + 1 = readFields acc 0 (y + 1)
                                | otherwise  = readFields (((curV,) <$> Map.lookup curV fields):acc) (x + 1) y
              where curV = V2 x y -- only consider same z level right now

--------------------
-- System Related --
--------------------

initWorldSpace :: forall w g. (Get w IO EntityCounter, Get w IO (Tiles g), Has w IO Physics, Lua.HasScripting w IO, ReadWriteEach w IO '[Existance, Local, LifeTime, Render g, Variables, WireReceivers, WorldSpace g, WorldStases])
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
    tiles :: Tiles g <- get global

    cmap $ \(Local _)   -> ws^.loadPoint.to Position
    -- we place all items in the world as entities
    forM_ (ws^.worldItems) $ \(InstancedItem item pos) -> newEntityFromItem item pos
    forM_ (ws^.worldEntities) $ \(InstancedPrototype _ proto pos sigOut sigIn config) -> do
        ety <- mkEntity (\s -> s {Lua.instanceConfig = config}) proto
        ety $= Position pos

        initialScript <- getIfExists ety
        forM_ initialScript $
            set ety . Lua.setInstanceConfig config 

        forM_ sigOut $ \sig -> modify ety (Lua.addWireController sig)

        forM_ sigIn $ \sig -> do
            maybeScript <- getIfExists ety
            forM_ maybeScript $ \script ->
                 when (Lua.shouldScriptRun onSignalChange script) $ Lua.addWireReceiver sig script
    
    let worldCollisionFilter = groupCollisionFilter Movement
        addWorldCollision polygons
            | Vec.null polygons = pure ()
            | otherwise         = do
                ety <- newExistingEntity (StaticBody, Position (V2 0 0))
                ety $= (Shape ety (Convex (Vec.head polygons) 0), worldCollisionFilter)
                
                mapM_ (\p -> newExistingEntity (Shape ety $ Convex p 0)) . Vec.tail $ polygons
    colPolys <- mkCollisionPolygons tiles $ ws^.worldFields

    addWorldCollision colPolys

    case ws^.worldScript of
        Nothing    -> pure ()
        (Just scr) -> do
            ety    <- newExistingEntity ()
            active <- loadScript ety scr
            ety    $= active

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
    
instance ToJSON (WorldSpace g) where
    toJSON (WorldSpace wid wname initPal palettes loadPos shouldSavePos vars script etys items _ legend) = object
        [ "world-id"                   .= wid
        , "name"                       .= wname
        , "script"                     .= (Lua.scriptID <$> script)
        , "initial-palette"            .= initPal
        , "palettes"                   .= Map.keys palettes
        , "load-point"                 .= (loadPos / unitsPerTile)
        , "should-save-exact-position" .= shouldSavePos
        , "legend"                     .= (_tileTextID <$> legend)
        -- map
        , "variables"                  .= vars
        , "entities"                   .= etys
        , "items"                      .= items
        ]

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
        variables  <- v .:? "global-variables" .!= Map.empty

        shouldSerializePosition      <- v .:? "should-save-exact-position" .!= False
        foregroundT :: Vector [Char] <- v .: "tiles"
        
        wScript :: Dependency s m (Maybe Lua.Script) <- flattenDependency <$> v .:? "script"

        legend :: Dependency s m (Map Char (Tile g)) <- do
            (keys, vals) :: ([Text], [Identifier]) <- unzip . Map.toList <$> v .: "legend"

            pure . fmap (Map.fromList . zip (T.head <$> keys))
                 . flattenDependency
                 . fmap (dependencyMapLookupElseError "Tile")
                 $ vals

        items    <- v .: "items"
        entities <- v .: "entities"

        let palettes = dependencyMapLookupMap (paletteIDs :: Vector Identifier)
            
            failIfNothing = error "Attempted to tile without a listing in the WorldSpace's legend"

            mkForegroundTiles lgnd = Vec.fromList . fmap (fromMaybe failIfNothing . flip Map.lookup lgnd) <$> foregroundT

            newFieldSet :: Dependency s m FieldSet
            newFieldSet = (mkForegroundTiles <$> legend) >>= liftDependency . fromTileVector2D

        pure $ WorldSpace worldId wName initPal <$> palettes <*> pure loadPnt <*> pure shouldSerializePosition <*> pure variables <*> wScript <*> entities <*> items <*> newFieldSet <*> legend
    parseJSON e          = typeMismatch "WorldSpace" e