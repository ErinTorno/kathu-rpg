module Kathu.Init
    ( entityWorld
    , system
    , sdlWindowConfig
    ) where

import           Apecs
import           Apecs.Physics
import           Control.Lens                    hiding (Identity)
import           Control.Monad                   (void)
import qualified Data.Map                        as Map
import qualified Data.Text                       as T
import           Foreign.C.String
import qualified SDL
import qualified SDL.Raw.Basic                   as SDLRaw
import qualified SDL.Raw.Enum                    as SDLRaw
import qualified SDL.Video                       as SDLV
import qualified System.Random                   as R
import           Verda.Graphics.Components       (Resolution(..), defaultCamera)
import           Verda.Graphics.Fonts            (fontID, initFontCache)
import           Verda.Graphics.Icons            (setWindowIcon)
import           Verda.Graphics.Sprites
import           Verda.System.Tile.Renderer      (addTileRendererExtension)
import           Verda.Util.Containers           (fromJustElseError)
import           Verda.Util.Types                (unID)
import           Verda.World                     (addBeforeRenderExtension, initVerdaWorld)

import           Kathu.Config.Dictionary
import           Kathu.Config.Settings
import           Kathu.Entity.Action
import           Kathu.Entity.Components
import           Kathu.Entity.Physics.Floor
import           Kathu.Entity.System
import           Kathu.Game                      (initPhysics)
import           Kathu.Graphics.Debug            (addDebugExtension)
import           Kathu.Graphics.UI               (addUIExtension, gameIcon)
import           Kathu.Language
import           Kathu.Random
import           Kathu.Scripting.Lua.Component   (registerComponentFunctions)
import           Kathu.Scripting.Lua.Global      (registerGlobalFunctions)
import           Kathu.Scripting.Lua             (LuaModules(..), initScripting)
import           Kathu.Scripting.Variables       (initVariables)
import           Kathu.World.Loader              (loadWorldSpace)
import           Kathu.World.Tile                (Tile, makeAllTiles)
import           Kathu.World.WorldSpace          (emptyWorldSpace)

entityWorld :: IO KathuWorld
entityWorld = initKathuWorld

-- initializes an entity as the local player
initLocalPlayer :: Entity -> SystemT' IO ()
initLocalPlayer ety =
    ety $= (defaultCamera, Local emptyActionPressed, emptyActionSet, Player)

initLanguage :: SDL.Window -> SDL.Renderer -> Settings -> Dictionary -> SystemT' IO ()
initLanguage window renderer settings dictionary = do
    let maybeLang        = dictionary^.dictLanguages.to (Map.lookup $ language settings)
        missingLangMsg   = "Language \"" `T.append` (unID . language $ settings) `T.append` "\" could not be found. Have the files been moved?"

        promptAndDefault = case dictionary^.dictLanguages.to (Map.lookup "english") of
            Just e -> do
                let msg = missingLangMsg `T.append` " Defaulting to English."

                SDLV.showSimpleMessageBox (Just window) SDLV.Warning "Missing Language" msg

                liftIO $ saveSettings (settings {language = "english"})
                pure e
            Nothing -> do
                let msg = missingLangMsg `T.append` " Default is missing too, unable to run."

                SDLV.showSimpleMessageBox (Just window) SDLV.Error "Missing Language" msg
                pure $ error "No default language could be found"

    lang   <- maybe promptAndDefault pure maybeLang
    global $= lang
    let missingFontErr lid = concat ["Font ", show lid, " required for language ", show $ langID lang, " but not found"]
        findFont lid       = fromJustElseError (missingFontErr lid) $ Map.lookup lid (dictionary^.dictFonts)
        curFonts = Map.fromList . map (\f -> (fontID f, f)) . Map.elems . fmap findFont . langFontIDs $ lang
    fontCache <- initFontCache renderer curFonts
    global    $= fontCache

system :: Settings -> SDL.Window -> SDL.Renderer -> SystemT' IO ()
system settings window renderer = do
    SDL.HintRenderScaleQuality SDL.$= SDL.ScaleNearest
    lift sdlWindowConfig
    initVerdaWorld
    -- important to register before dictionary, as dictionary will be loading many scripts
    world <- ask
    global $= LuaModules [registerComponentFunctions world, registerGlobalFunctions world]
    -- now that lua modules are initialized, good to load
    (dictionary, manager) <- liftIO $ loadDictionary renderer
    seed       <- lift . maybe (R.randomIO :: IO Int) pure . randomSeed $ settings
    tilesV     <- lift . makeAllTiles . view dictTiles $ dictionary
    variables  <- initVariables
    initScripting
    global $= variables
    global $= dictionary
    global $= manager
    global $= Random (R.mkStdGen seed)
    global $= tilesV
    global $= settings
    global $= dictionary^.dictUIConfig
    global $= (Gravity $ V2 0 0) -- no gravity, as the game is top-down
    global $= Resolution (fromIntegral <$> resolution settings)
    addTileRendererExtension (Proxy :: Proxy Tile)
    addDebugExtension
    addUIExtension
    addAnimationExtension

    initLanguage window renderer settings dictionary

    setWindowIcon window (dictionary^.dictUIConfig.to gameIcon)

    floorPropEtys <- mapM initFloorProperty . view dictFloorProperties $ dictionary
    global  $= FloorProperties (floorPropEtys Map.! "default") floorPropEtys
    
    playerEty <- newFromPrefab . fromJustElseError "No player entity config was loaded" $ dictionaryLookup dictionary dictPrefabs "player"
    initLocalPlayer playerEty

    loadWorldSpace $ case initialWorld settings of
        Nothing -> emptyWorldSpace
        Just ws -> fromJustElseError ("No worldspace config with ID " ++ show ws ++ " was loaded") $ dictionaryLookup dictionary dictWorldSpaces ws

    initPhysics
    pure ()

sdlWindowConfig :: IO ()
sdlWindowConfig =
    -- very important if it editor mode, as we want to be able to click an option in the editor window,
    -- and then immediately place it without needing to double-click in the game window
    withCString "SDL_MOUSE_FOCUS_CLICKTHROUGH" $ \hintStr ->
        withCString "1" $ \enabledStr ->
            void $ SDLRaw.setHintWithPriority hintStr enabledStr SDLRaw.SDL_HINT_OVERRIDE

-- TODO move ot Kathu.Graphics once exec and library are merged
addAnimationExtension :: SystemT' IO ()
addAnimationExtension = addBeforeRenderExtension $ \_ _ ->
    cmap $ \(sprite, ActionSet {_moving = mv, _facingDirection = fac}) -> case mv of
            Nothing -> setAnimationID (dirToAnimIndex fac) sprite
            Just dir -> if
                | dirIdx == getAnimationID sprite -> sprite
                | otherwise                       -> setAnimationID dirIdx sprite
                where dirIdx = dirToAnimIndex dir