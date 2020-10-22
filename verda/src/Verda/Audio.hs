{-# LANGUAGE UndecidableInstances #-}

module Verda.Audio
    ( Sound(..)
    , Music(..)
    , pattern SDLM.Once
    , pattern SDLM.Forever
    , playSound
    , playMusic
    , fadeInMusic
    , fadeOutMusic
    ) where

import           Control.Monad                (void)
import           Control.Monad.IO.Class       (MonadIO)
import           Data.Aeson
import qualified Data.Map                     as Map
import qualified Data.Text                    as T
import qualified SDL.Mixer                    as SDLM

import           Verda.IO.Directory
import           Verda.Parsing.Counting
import           Verda.Util.Dependency
import           Verda.Util.Types

newtype Sound = Sound {unSound :: SDLM.Chunk}

newtype Music = Music {unMusic :: SDLM.Music}

playSound :: MonadIO m => Sound -> m ()
playSound = SDLM.play . unSound

playMusic :: MonadIO m => SDLM.Times -> Music -> m ()
playMusic times = SDLM.playMusic times . unMusic

fadeInMusic :: MonadIO m => Int -> SDLM.Times -> Music -> m ()
fadeInMusic ms times = SDLM.fadeInMusic ms times . unMusic

fadeOutMusic :: MonadIO m => Int -> m ()
fadeOutMusic ms = void $ SDLM.fadeOutMusic ms

-------------------
-- Serialization --
-------------------

instance ( s `CanProvide` WorkingDirectory
         , s `CanStoreEach` '[CountingIDs, IDMap Sound]
         , MonadIO m
         ) => FromJSON (Dependency s m Sound) where
    parseJSON = withText "Sound" $ \t -> pure $ do
        urlStr   <- resolveAssetPathDP . T.unpack $ t
        let urlID = mkIdentifier . T.pack $ urlStr
        depMap   <- provide
        case Map.lookup urlID depMap of
            Just s  -> pure $ s
            Nothing -> do
                s <- fmap Sound . liftDependency . SDLM.load @SDLM.Chunk $ urlStr
                dependencyMapInsert urlID s
                pure s

instance ( s `CanProvide` WorkingDirectory
         , s `CanStoreEach` '[CountingIDs, IDMap Music]
         , MonadIO m
         ) => FromJSON (Dependency s m Music) where
    parseJSON = withText "Music" $ \t -> pure $ do
        urlStr   <- resolveAssetPathDP . T.unpack $ t
        let urlID = mkIdentifier . T.pack $ urlStr
        depMap   <- provide
        case Map.lookup urlID depMap of
            Just s  -> pure $ s
            Nothing -> do
                s <- fmap Music . liftDependency . SDLM.load @SDLM.Music $ urlStr
                dependencyMapInsert urlID s
                pure s