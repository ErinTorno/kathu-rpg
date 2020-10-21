module Kathu.App.Init
    ( entityWorld
    , system
    , sdlWindowConfig
    ) where

import           Apecs                           hiding (get)
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

import           Kathu.App.Data.Controls
import           Kathu.App.Data.Dictionary
import           Kathu.App.Data.Settings
import           Kathu.App.Graphics.Font         (initFontCache)
import           Kathu.App.Graphics.ImageManager
import           Kathu.App.Graphics.UI
import           Kathu.App.System
import           Kathu.App.World                 (loadWorldSpace)
import           Kathu.Entity.Action
import           Kathu.Entity.Components
import           Kathu.Entity.Physics.Floor
import           Kathu.Entity.System
import           Kathu.Game                      (initPhysics)
import           Kathu.Graphics.Camera
import           Kathu.Language
import           Kathu.Scripting.Lua             (initScripting)
import           Kathu.Scripting.Variables       (initVariables)
import           Verda.Util.Containers           (fromJustElseError)
import           Kathu.World.WorldSpace          (emptyWorldSpace)
import           Verda.Util.Types                (unID)

entityWorld :: IO EntityWorld
entityWorld = initEntityWorld

-- initializes an entity as the local player
initLocalPlayer :: Entity -> SystemT' IO ()
initLocalPlayer ety =
    ety $= (Camera 1.0, Local emptyActionPressed, emptyActionSet, Player)

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

    lang      <- maybe promptAndDefault pure maybeLang
    fontCache <- initFontCache renderer $ langFonts lang
    global    $= fontCache

system :: SDL.Window -> SDL.Renderer -> Settings -> SystemT' IO ()
system window renderer settings = do
    (dictionary, manager) <- liftIO $ loadDictionary renderer
    seed       <- lift . maybe (R.randomIO :: IO Int) pure . randomSeed $ settings
    tilesV     <- lift . makeTiles . view dictTiles $ dictionary
    variables  <- initVariables
    controlST  <- mkControlState
    initScripting
    global $= variables
    global $= dictionary
    global $= manager
    global $= Random (R.mkStdGen seed)
    global $= tilesV
    global $= settings
    global $= controlST
    global $= dictionary^.dictUIConfig
    global $= (Gravity $ V2 0 0) -- no gravity, as the game is top-down

    initLanguage window renderer settings dictionary

    setWindowIcon window (dictionary^.dictUIConfig.to gameIcon)

    floorPropEtys <- mapM initFloorProperty . view dictFloorProperties $ dictionary
    global  $= FloorProperties (floorPropEtys Map.! "default") floorPropEtys
    
    playerEty <- newFromPrototype . fromJustElseError "No player entity config was loaded" $ dictionaryLookup dictionary dictPrototypes "player"
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