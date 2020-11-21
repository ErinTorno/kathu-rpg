module Kathu.World.Loader where

import           Apecs                     hiding (Map)
import           Apecs.Physics             hiding (Map)
import           Control.Lens              hiding (set)
import           Control.Monad             (forM_, void, when)
import           Control.Monad.IO.Class    (MonadIO)
import qualified Data.Vector               as Vec
import           Verda.Graphics.SpriteManager (loadPalettes)
import           Verda.Util.Apecs
import           Verda.World               (Existance(..))

import           Kathu.Editor.Tools.Info
import           Kathu.Entity.Components
import           Kathu.Entity.Item
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Physics.CollisionGroup
import           Kathu.Entity.System
import           Kathu.Scripting.Event
import qualified Kathu.Scripting.Lua       as Lua
import           Kathu.Scripting.Variables
import           Kathu.Scripting.Wire
import           Kathu.World.ChunkBuilder
import           Kathu.World.Stasis
import           Kathu.World.Tile          (AllTiles')
import           Kathu.World.WorldSpace

loadWorldSpace :: WorldSpace -> SystemT' IO ()
loadWorldSpace ws = do
    saveWorldVariables ws
    -- we clean up all previous entities without lifetimes
    cmapM_ $ \(Existance, _ :: Not LifeTime, ety) -> destroyEntity ety
    -- for those that have them, we clear those that are not persistant
    cmapM_ $ \(lf :: LifeTime, ety)    -> when (lf /= Persistant) $ destroyEntity ety
    prevManager <- get global
    manager <- loadPalettes (ws^.worldPalettes) prevManager
    global  $= manager

    global $= ws

    cmap $ \(_ :: Player) -> ws^.loadPoint.to Position
    -- we place all items in the world as entities
    forM_ (ws^.worldItems) $ \(InstancedItem item pos) -> newEntityFromItem item pos
    forM_ (ws^.worldEntities) placeInstancedPrefab

    global $= ws^.worldChunks
    buildTileCollisions ws

    case ws^.worldScript of
        Nothing    -> pure ()
        (Just scr) -> do
            ety    <- newExistingEntity ()
            active <- Lua.loadScript id ety scr
            ety    $= active
    void $ setPalette (ws^.initialPalette)
    -- to prevent pausing issues during gameplay, we force a GC now while it's just done loading
    runGC

placeInstancedPrefab :: InstancedPrefab -> SystemT' IO ()
placeInstancedPrefab instancedPrefab@(InstancedPrefab _ prefab pos sigOut sigIn config) = do
    IncludeEditorInfo shouldIncludeInfo <- get global
    ety <- newFromPrefabWithScriptMapping (\s -> s {Lua.instanceConfig = config}) prefab
    ety $= Position pos
    -- include the instance with the entity, as we are running in some form of editor mode
    when shouldIncludeInfo $
        ety $= EditorInstancedFromWorld instancedPrefab

    initialScript <- getIfExists ety
    forM_ initialScript $
        set ety . Lua.setInstanceConfig config 

    forM_ sigOut $ \sig -> modify ety (addWireController sig)

    forM_ sigIn $ \sig -> do
        maybeScript <- getIfExists ety
        forM_ maybeScript $ \script ->
                when (Lua.shouldScriptRun onSignalChange script) $ addWireReceiver sig script

rebuildCurrentTileCollisions :: SystemT' IO ()
rebuildCurrentTileCollisions = do
    worldspace :: WorldSpace <- get global
    destroyTileCollisions
    buildTileCollisions worldspace

-- | Destroy all entities created for holding tile collisions
destroyTileCollisions :: SystemT' IO ()
destroyTileCollisions =
    cmapM_ $ \(specialEty, ety) -> when (specialEty == WorldCollision) $ destroyEntity ety

buildTileCollisions :: WorldSpace -> SystemT' IO ()
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
saveWorldVariables :: MonadIO m => WorldSpace -> SystemT' m ()
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
newEntityFromItem :: MonadIO m => ItemStack -> V2 Double -> SystemT' m Entity
newEntityFromItem stack v = newExistingEntity (StaticBody, Position v, itemIcon . stackItem $ stack)
