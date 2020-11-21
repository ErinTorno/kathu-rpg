{-# LANGUAGE RecordWildCards #-}

module Kathu.Entity.Utils where
-- Not great temp module
-- TODO move into better modules once done shifting around

import           Apecs
import           Control.Lens
import           Control.Monad                   (forM_, void)
import qualified Data.Map                        as Map
import           Kathu.Entity.Physics.BodyConfig (setBodyConfig)
import           Kathu.Entity.Prefab
import           Verda.Graphics.SpriteManager    (setPaletteManager)
import           Verda.Util.Apecs
import           Verda.Util.Types
import           Verda.World

import           Kathu.Config.Dictionary         (dictPrefabs)
import           Kathu.Entity.System
import qualified Kathu.Scripting.Lua             as Lua
import           Kathu.Scripting.Lua.Component
import           Kathu.Scripting.Lua.Global

-- TODO move this to Graphics when palettes are re-implemented to work with Verda
setPalette :: Identifier -> SystemT' IO Bool
setPalette = setPaletteManager

destroyEntity :: Entity -> SystemT' IO ()
destroyEntity ety = do
    whenExists ety Lua.releaseActiveScript
    destroy ety (Proxy @AllComponents)

loadKathuScript :: (Lua.ActiveScript -> Lua.ActiveScript) -> Entity -> Lua.Script -> SystemT' IO Lua.ActiveScript
loadKathuScript = Lua.loadScript [registerComponentFunctions, registerGlobalFunctions lookupEntityPrefab newFromPrefab setPalette]

newFromPrefabWithScriptMapping :: (Lua.ActiveScript -> Lua.ActiveScript) -> Prefab -> SystemT' IO Entity
newFromPrefabWithScriptMapping f Prefab{..} = do
    ety <- newEntity (Existance, pIdentity)
    forM_ pActorState    (ety$=)
    forM_ pInventory     (ety$=)
    forM_ pLifeTime      (ety$=)
    forM_ pMovingSpeed   (ety$=)
    forM_ pSprite        (ety$=)
    forM_ pSpecialEntity (ety$=)
    forM_ pTags          (ety$=)
    forM_ pScript        (void . loadKathuScript f ety)
    setBodyConfig ety pBodyConfig
    return ety

newFromPrefab :: Prefab -> SystemT' IO Entity
newFromPrefab = newFromPrefabWithScriptMapping id

lookupEntityPrefab :: Identifier -> SystemT' IO (Maybe Prefab)
lookupEntityPrefab idt = Map.lookup idt . (^.dictPrefabs) <$> get global