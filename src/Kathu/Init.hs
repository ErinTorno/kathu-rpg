{-# LANGUAGE OverloadedStrings #-}

module Kathu.Init where

import Apecs hiding (($=))
import Control.Monad
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
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

system :: Settings -> SystemT' IO ()
system settings = do
    library <- lift (loadLibrary assetPath)
    seed    <- lift (R.randomIO :: IO Int)
    set global $ library
    set global $ Random (R.mkStdGen seed)
    set global $ settings

    -- testing set for now; will change in future to be else where

    playerEty <- newFromPrototype $ (prototypes library) Map.! "player"
    localPlayer settings playerEty
    
    boulderEty <- newFromPrototype $ (prototypes library) Map.! "boulder"

    pure ()

openGL :: IO (GL.Program, GL.AttribLocation)
openGL = do -- compile vertex shader
    vs <- GL.createShader GL.VertexShader
    GL.shaderSourceBS vs $= vsSource
    GL.compileShader vs
    vsOK <- GL.get $ GL.compileStatus vs
    unless vsOK $ do
        hPutStrLn stderr "Error in vertex shader\n"
        exitFailure

    -- Do it again for the fragment shader
    fs <- GL.createShader GL.FragmentShader
    GL.shaderSourceBS fs $= fsSource
    GL.compileShader fs
    fsOK <- GL.get $ GL.compileStatus fs
    unless fsOK $ do
        hPutStrLn stderr "Error in fragment shader\n"
        exitFailure

    program <- GL.createProgram
    GL.attachShader program vs
    GL.attachShader program fs
    GL.attribLocation program "coord2d" $= GL.AttribLocation 0
    GL.linkProgram program
    linkOK <- GL.get $ GL.linkStatus program
    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    unless (linkOK && status) $ do
        hPutStrLn stderr "GL.linkProgram error"
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure
    GL.currentProgram $= Just program
        
    return (program, GL.AttribLocation 0)


vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n" ["void main() { gl_Position = gl_Vertex; }"]
fsSource = BS.intercalate "\n" ["void main() { gl_FragColor = vec4(1,1,0,1); }"]