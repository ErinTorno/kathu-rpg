{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}

module Kathu.World.WorldSpace where

import           Apecs                     hiding (Map)
import           Apecs.Physics             hiding (Map)
import           Control.Monad             (foldM, forM_, when)
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
import           Kathu.Entity.Prototype    (EntityPrototype)
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
import           Kathu.Util.Flow           ((>>>=))
import           Kathu.Util.Types          (Identifier, IDMap)
import           Kathu.World.Field
import           Kathu.World.Stasis
import           Kathu.World.Tile          (Tile)

data InstancedPrototype g = InstancedPrototype
    { basePrototype      :: EntityPrototype g
    , spawnLocation      :: V2 Double
    , wireSignalEmitter  :: !(Maybe Identifier)
    , wireSignalReceiver :: !(Maybe Identifier)
    , instanceConfig     :: !(IDMap WorldVariable)
    }

data WorldSpace g = WorldSpace
    { worldID            :: !Identifier
    , worldName          :: !Text
    , initialPalette     :: !Identifier
    , worldPalettes      :: !(IDMap Palette)
    , loadPoint          :: !(V2 Double)
    , shouldSavePosition :: !Bool -- when serialized, should we remember where the player was?
    , worldVariables     :: IDMap WorldVariable
    , worldScript        :: !(Maybe Lua.Script)
    , worldEntities      :: Vector (InstancedPrototype g)
    , worldItems         :: Vector (V2 Double, ItemStack g)
    , worldFields        :: !FieldSet
    }

emptyWorldSpace :: WorldSpace g
emptyWorldSpace = WorldSpace "" "the void..." "" Map.empty (V2 0 0) False Map.empty Nothing Vec.empty Vec.empty Map.empty

-- right now we only consider horizontal fields; ones with different z depths are ignored
fieldsSurrounding :: RealFrac a => V2 a -> WorldSpace g -> [(V2 Int, Field)]
fieldsSurrounding v ws = catMaybes $ readFields [] (ox - 1) (oy - 1)
    where fields    = worldFields ws
          (# ox, oy #) = fieldContainingCoordV2 v
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

    cmap $ \(Local _)   -> Position . loadPoint $ ws
    -- we place all items in the world as entities
    forM_ (worldItems ws) $ \(pos, item) -> newEntityFromItem item pos
    forM_ (worldEntities ws) $ \(InstancedPrototype proto pos sigOut sigIn config) -> do
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
    colPolys <- mkCollisionPolygons tiles . worldFields $ ws

    addWorldCollision colPolys

    case worldScript ws of
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

    let oldID = worldID oldWS

    -- if we already have this world saved, we get its saved variables; otherwise we use the default ones
    let newVars = maybe (worldVariables newWS) statisVariables . getStasis oldID $ stases

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

-- Instances and functions related to serializing the WorldSpace
-- This is kept at the end due to the length and complexity of theses

instance ( s `CanProvide` (IDMap (EntityPrototype g))
         , s `CanProvide` (IDMap (Tile g))
         , s `CanProvide` WorkingDirectory
         , FromJSON (Dependency s m (ItemStack g))
         , MonadIO m
         ) => FromJSON (Dependency s m (WorldSpace g)) where
    parseJSON (Object v) = do
        worldId   <- v .: "world-id"
        wName     <- v .: "name"
        initPal   <- v .: "initial-palette"
        palettes  <- v .:? "palettes" .!= Map.empty
        loadPnt   <- (*unitsPerTile) <$> v .: "load-point"
        variables <- v .:? "global-variables" .!= Map.empty

        shouldSerializePosition      <- v .:? "should-save-exact-position" .!= False
        foregroundT :: Vector [Char] <- v .: "tiles"
        
        wScript :: Dependency s m (Maybe Lua.Script) <- flattenDependency <$> v .:? "script"

        legend :: Dependency s m (Map Char (Tile g)) <- do
            (keys, vals) :: ([Text], [Identifier]) <- unzip . Map.toList <$> v .: "legend"

            return . fmap (Map.fromList . zip (T.head <$> keys))
                   . flattenDependency
                   . fmap (dependencyMapLookupElseError "Tile")
                   $ vals

        let parsePlacement pidiv fn (Array vec) = foldM (pidiv fn) (return []) vec
            parsePlacement _ _ e            = typeMismatch "Placement" e

            parseIndivEntityPlace fn acc val@(Object obj) = do
                pos     <- (*unitsPerTile) <$> obj .: "position"
                sigEmit <- obj .:? "emitting-signal"
                sigRece <- obj .:? "receiving-signal"
                config  <- obj .:? "config" .!= Map.empty
                stack   <- fn val
                return $ acc >>= \ls -> (:ls) . (\e -> InstancedPrototype e pos sigEmit sigRece config) <$> stack
            parseIndivEntityPlace _ _ e                 = typeMismatch "Placement" e

            parseIndivPlace fn acc val@(Object obj) = do
                pos   <- (*unitsPerTile) <$> obj .: "position"
                stack <- fn val
                return $ acc >>= \ls -> (:ls) . (pos,) <$> stack
            parseIndivPlace _ _ e                 = typeMismatch "Placement" e

            parseEty = withObject "EntityPlacement" $ \obj ->
                dependencyMapLookupElseError "Entity" <$> (obj .: "entity" :: Parser Identifier)

        items    <- v .: "items"    >>= parsePlacement parseIndivPlace parseJSON       >>>= return . Vec.fromList
        entities <- v .: "entities" >>= parsePlacement parseIndivEntityPlace parseEty  >>>= return . Vec.fromList

        let failIfNothing = error "Attempted to tile without a listing in the WorldSpace's legend"

            mkForegroundTiles lgnd = Vec.fromList . fmap (fromMaybe failIfNothing . flip Map.lookup lgnd) <$> foregroundT

            newFieldSet :: Dependency s m FieldSet
            newFieldSet = (mkForegroundTiles <$> legend) >>= liftDependency . fromTileVector2D

        return $ WorldSpace worldId wName initPal palettes loadPnt shouldSerializePosition variables <$> wScript <*> entities <*> items <*> newFieldSet
    parseJSON e          = typeMismatch "WorldSpace" e