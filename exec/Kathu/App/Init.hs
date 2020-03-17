{-# LANGUAGE OverloadedStrings #-}

module Kathu.App.Init
    ( entityWorld
    , system
    ) where

import           Apecs                           hiding (get)
import           Apecs.Physics
import           Control.Lens                    hiding (Identity)
import qualified Data.Map                        as Map
import qualified Data.Text                       as T
import           Linear.V2                       (V2(..))
import qualified SDL
import qualified SDL.Video                       as SDLV
import qualified System.Random                   as R

import           Kathu.App.Data.Library
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
import           Kathu.IO.Directory              (assetPath)
import           Kathu.Game                      (initPhysics)
import           Kathu.Graphics.Camera
import           Kathu.Language
import           Kathu.Scripting.Lua             (initScripting)
import           Kathu.Scripting.Variables       (initVariables)
import           Kathu.Util.Types                (unID)

entityWorld :: IO EntityWorld
entityWorld = initEntityWorld

-- initializes an entity as the local player
initLocalPlayer :: Entity -> SystemT' IO ()
initLocalPlayer ety = do
    ety $= Camera 1.0
    ety $= Local emptyActionPressed
    ety $= emptyActionSet

initLanguage :: SDL.Window -> SDL.Renderer -> Settings -> Library -> SystemT' IO ()
initLanguage window renderer settings library = do
    let maybeLang        = library^.languages.to (Map.lookup $ language settings)
        missingLangMsg   = "Language \"" `T.append` (unID . language $ settings) `T.append` "\" could not be found. Have the files been moved?"

        promptAndDefault = case library^.languages.to (Map.lookup "english") of
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
    (library, surfaces) <- lift . loadLibrary mempty $ assetPath
    seed       <- lift . maybe (R.randomIO :: IO Int) pure . randomSeed $ settings
    manager    <- lift . mkImageManager renderer $ surfaces
    tilesV     <- lift . makeTiles . view tiles $ library
    variables  <- initVariables
    initScripting
    global  $= variables
    global  $= library
    global  $= manager
    global  $= Random (R.mkStdGen seed)
    global  $= tilesV
    global  $= settings
    global  $= library^.uiConfig
    global  $= (Gravity $ V2 0 0) -- no gravity, as the game is top-down

    initLanguage window renderer settings library

    setWindowIcon window (library^.uiConfig.to gameIcon)

    floorPropEtys <- mapM initFloorProperty . view floorProperties $ library
    global  $= FloorProperties (floorPropEtys Map.! "default") floorPropEtys

    let getLib g t = (library^.g) Map.! t
    
    playerEty <- newFromPrototype $ getLib prototypes "player"
    initLocalPlayer playerEty

    let worldspace = getLib worldSpaces . initialWorld $ settings
    loadWorldSpace worldspace

    initPhysics
    
    pure ()