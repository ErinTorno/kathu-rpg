module Verda.Graphics.Shaders (ShaderSet(..), mkShaderSet) where

import           Apecs                                hiding (Map, ($=))
import           Control.Monad                        (forM_, unless)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Aeson
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as BS
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import qualified Graphics.Rendering.OpenGL.GL.Shaders as GL
import           SDL                                  (($=))

import           Verda.IO.Directory
import           Verda.Util.Dependency

newtype ShaderSet = ShaderSet {getProgram :: Maybe GL.Program}

instance Semigroup ShaderSet where (<>) = mappend
instance Monoid ShaderSet where mempty = ShaderSet Nothing
instance Component ShaderSet where type Storage ShaderSet = Global ShaderSet

instance ( s `CanProvide` WorkingDirectory
         , MonadIO m
         ) => (FromJSON (Dependency s m ShaderSet)) where
    parseJSON = withObject "ShaderSet" $ \v -> do
        compFileDP <- flattenDependency . fmap resolveAssetPathDP <$> v .:? "compute"
        fragFileDP <- flattenDependency . fmap resolveAssetPathDP <$> v .:? "fragment"
        geomFileDP <- flattenDependency . fmap resolveAssetPathDP <$> v .:? "geometry"
        vertFileDP <- flattenDependency . fmap resolveAssetPathDP <$> v .:? "vertex"
        pure $ do
            compFile <- compFileDP
            fragFile <- fragFileDP
            geomFile <- geomFileDP
            vertFile <- vertFileDP
            liftDependency . liftIO $ do
                shaderMap <- mapM BS.readFile
                           . Map.mapMaybe id
                           . Map.fromList
                           $ [(GL.ComputeShader, compFile), (GL.FragmentShader, fragFile), (GL.GeometryShader, geomFile), (GL.VertexShader, vertFile)]
                mkShaderSet shaderMap

mkShaderSet :: MonadIO m => Map GL.ShaderType ByteString -> m ShaderSet
mkShaderSet shaders = liftIO $ do
    prog <- GL.createProgram
    forM_ (Map.toList shaders) $ \(typ, src) -> do
        shader <- mkShader typ src
        GL.attachShader prog shader
    GL.linkProgram prog
    GL.validateProgram prog
    isValid <- GL.validateStatus prog
    unless isValid $ do
        err <- GL.programInfoLog prog
        error err
    pure . ShaderSet $ Just prog

mkShader :: MonadIO m => GL.ShaderType -> ByteString -> m GL.Shader
mkShader typ src = liftIO $ do
    shader <- GL.createShader typ
    GL.shaderSourceBS shader $= src
    GL.compileShader shader

    isCompiled <- GL.compileStatus shader
    unless isCompiled $ do
        err <- GL.shaderInfoLog shader
        putStrLn $ "Unable to compile shader of type " ++ show typ
        print err
    pure shader