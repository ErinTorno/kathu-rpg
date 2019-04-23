{-# LANGUAGE OverloadedStrings #-}

module Kathu.Init (entityWorld, localPlayer, system, openGL) where

import Apecs hiding (($=), get)
import Control.Monad
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V
import Graphics.Rendering.OpenGL
import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.System
import Kathu.IO.File (assetPath)
import Kathu.IO.Library
import Kathu.IO.Settings
import qualified SDL
import SDL (($=))
import System.Exit (exitFailure)
import System.IO
import qualified System.Random as R

entityWorld = initEntityWorld

-- initializes an entity as the local player
localPlayer :: Settings -> Entity -> SystemT' IO ()
localPlayer settings ety = do
    set ety $ Camera (resolutionX settings) (resolutionY settings) 1.0
    set ety $ Local emptyActionPressed
    set ety $ emptyActionSet

system :: SDL.Renderer -> Settings -> SystemT' IO ()
system renderer settings = do
    library <- lift (loadLibrary renderer assetPath)
    seed    <- lift (R.randomIO :: IO Int)
    set global $ library
    set global $ Random (R.mkStdGen seed)
    set global $ settings

    -- testing set for now; will change in future to be else where

    playerEty <- newFromPrototype $ (prototypes library) Map.! "player"
    localPlayer settings playerEty
    
    --boulderEty <- newFromPrototype $ (prototypes library) Map.! "boulder"

    pure ()

openGL :: IO ()
openGL = do -- compile vertex shader
    initializeProgram
    texture Texture2D $= Enabled
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)


    -- textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    -- texture2DWrap $= (Repeated, ClampToEdge)

initializeProgram :: IO ()
initializeProgram = do
    vs <- createShader VertexShader
    shaderSourceBS vs $= vsSource
    compileShader vs
    vsOK <- get $ compileStatus vs
    unless vsOK $ do
        hPutStrLn stderr "Error in vertex shader\n"
        exitFailure

    -- Do it again for the fragment shader
    fs <- createShader FragmentShader
    shaderSourceBS fs $= fsSource
    compileShader fs
    fsOK <- get $ compileStatus fs
    unless fsOK $ do
        hPutStrLn stderr "Error in fragment shader\n"
        exitFailure

    program <- createProgram
    attachShader program vs
    attachShader program fs
    attribLocation program "coord2d" $= AttribLocation 0
    linkProgram program
    linkOK <- get $ linkStatus program
    validateProgram program
    status <- get $ validateStatus program
    unless (linkOK && status) $ do
        hPutStrLn stderr "linkProgram error"
        plog <- get $ programInfoLog program
        putStrLn plog
        exitFailure
    currentProgram $= Just program

vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n" ["void main() { gl_Position = gl_Vertex; }"]
fsSource = BS.intercalate "\n" ["void main() { gl_FragColor = vec4(1,1,0,1); }"]