{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
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
import           Control.Monad             (foldM, void, when)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Aeson
import           Data.Aeson.Types          (Parser, typeMismatch)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (catMaybes, fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vec
import           Linear.V2                 (V2(..))

import           Kathu.Entity.Components
import           Kathu.Entity.Item
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Prototype    (EntityPrototype)
import           Kathu.Entity.System       (Tiles)
import           Kathu.Graphics.Drawable   (Render)
import           Kathu.Graphics.Palette
import           Kathu.IO.Directory        (WorkingDirectory)
import qualified Kathu.Scripting.Lua       as Lua
import           Kathu.Scripting.Variables
import           Kathu.Util.Apecs
import           Kathu.Util.Dependency
import           Kathu.Util.Flow           ((>>>=))
import           Kathu.Util.Types          (Identifier, IDMap)
import           Kathu.World.Field
import           Kathu.World.Stasis
import           Kathu.World.Tile          (Tile)

data WorldSpace g = WorldSpace
    { worldID            :: !Identifier
    , worldName          :: !Text
    , initialPalette     :: !Identifier
    , worldPalettes      :: !(IDMap Palette)
    , loadPoint          :: !(V2 Double)
    , shouldSavePosition :: !Bool -- when serialized, should we remember where the player was?
    , worldVariables     :: IDMap WorldVariable
    , worldScript        :: !(Maybe Lua.Script)
    , worldEntities      :: Vector (V2 Double, EntityPrototype g)
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

initWorldSpace :: forall w m g. (MonadIO m , Get w m EntityCounter, Get w m (Tiles g), Has w m Physics, ReadWriteEach w m '[Existance, Local, LifeTime, Lua.ActiveScript, Render g, WorldSpace g, WorldStases])
               => (Entity -> SystemT w m ())
               -> (EntityPrototype g -> SystemT w m Entity)
               -> (Lua.Script -> SystemT w m Lua.ActiveScript)
               -> WorldSpace g
               -> SystemT w m ()
initWorldSpace destroyEty mkEntity loadScript ws = do
    -- we clean up all previous entities without lifetimes
    cmapM_ $ \(Existance, _ :: Not LifeTime, ety) -> destroyEty ety
    -- for those that have them, we clear those that are not persistant
    cmapM_ $ \(lf :: LifeTime, ety)    -> when (lf /= Persistant) $ destroyEty ety
    
    global $= ws
    tiles :: Tiles g <- get global

    cmap $ \(Local _)   -> (Position . loadPoint $ ws)
    -- we place all items in the world as entities
    mapM_ (\(pos, ety)  -> mkEntity ety >>= (flip ($=)) (Position pos)) (worldEntities ws)
    mapM_ (\(pos, item) -> newEntityFromItem item pos) (worldItems ws)
    
    let addWorldCollision polygons
            | Vec.null polygons = pure ()
            | otherwise         = do
                ety <- newExistingEntity (StaticBody, Position (V2 0 0))
                ety $= (Shape ety $ Convex (Vec.head polygons) 0)
                
                mapM_ (\p -> newExistingEntity (Shape ety $ Convex p 0)) . Vec.tail $ polygons
    colPolys <- mkCollisionPolygons tiles . worldFields $ ws

    addWorldCollision colPolys

    case worldScript ws of
        Nothing    -> pure ()
        (Just scr) -> do
            active <- loadScript scr
            void $ newExistingEntity active

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
        wScript :: Dependency s m (Maybe Lua.Script) <- flattenDependency <$> v .:? "script"
        initPal   <- v .: "initial-palette"
        palettes  <- v .:? "palettes" .!= Map.empty
        loadPnt   <- (*unitsPerTile) <$> v .: "load-point"
        shouldSerializePosition <- v .:? "should-save-exact-position" .!= False
        variables <- v .:? "global-variables" .!= Map.empty
        foregroundT :: [[Char]] <- v .: "tiles"
        legend      :: Dependency s m (Map Char (Tile g)) <- do
            (keys, vals) :: ([Text], [Identifier]) <- ((unzip . Map.toList) <$> v .: "legend")
            let dLookup = dependencyMapLookupElseError :: String -> Identifier -> Dependency s m (Tile g)
             in pure . fmap (Map.fromList . zip (T.head <$> keys)) . flattenDependency . fmap (dLookup "Tile") $ vals

        let parsePlacement fn (Array vec) = foldM (parseIndivPlace fn) (pure []) vec
            parsePlacement _ e            = typeMismatch "Placement" e
            parseIndivPlace fn acc val@(Object obj) = do
                pos   <- (*unitsPerTile) <$> obj .: "position"
                stack <- fn val
                pure $ acc >>= \ls -> ((:ls) . (pos,)) <$> stack
            parseIndivPlace _ _ e                 = typeMismatch "Placement" e
            parseEty = withObject "EntityPlacement" $ \obj -> (obj .: "entity" :: Parser Identifier) >>= pure . dependencyMapLookupElseError "Entity" 

        items    <- v .: "items" >>= parsePlacement parseJSON >>>= pure . Vec.fromList
        entities <- v .: "entities" >>= parsePlacement parseEty >>>= pure . Vec.fromList

        let failIfNothing = error "Attempted to tile without a listing in the WorldSpace's legend"
            applyLegend tileList2D = (\lgnd -> Vec.fromList . fmap Vec.fromList . fmap (fmap (fromMaybe failIfNothing . (flip Map.lookup) lgnd)) $ tileList2D) <$> legend
            newFieldSet :: Dependency s m (FieldSet)
            newFieldSet = applyLegend foregroundT >>= liftDependency . fromTileVector2D

        pure $ WorldSpace worldId wName initPal palettes loadPnt shouldSerializePosition variables <$> wScript <*> entities <*> items <*> newFieldSet
    parseJSON e          = typeMismatch "WorldSpace" e