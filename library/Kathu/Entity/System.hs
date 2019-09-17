{-# OPTIONS_GHC -fno-warn-orphans #-}
-- we need orphan instances to set up the Apecs system

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.Entity.System where

import Apecs hiding (Map)
import Control.Monad.IO.Class (MonadIO)
import Data.Semigroup (Semigroup)
import Data.Word
import qualified System.Random as R

import Kathu.World.Time (WorldTime(..))

-- Globals

-- these are kept separate, as the times for each type of loop may become desynced
-- ex: We may update the physics multiple times to "catch-up" if there was a delay in the system
--     Or update the graphics multiple times to display higher frames while we wait for the physics to run again
newtype  LogicTime = LogicTime (Word32) deriving (Show, Eq)
instance Semigroup LogicTime where (<>) = mappend
instance Monoid LogicTime where mempty = LogicTime 0
instance Component LogicTime where type Storage LogicTime = Global LogicTime

newtype  RenderTime = RenderTime (Word32) deriving (Show, Eq)
instance Semigroup RenderTime where (<>) = mappend
instance Monoid RenderTime where mempty = RenderTime 0
instance Component RenderTime where type Storage RenderTime = Global RenderTime

instance Semigroup WorldTime where (<>) = mappend
instance Monoid WorldTime where mempty = WorldTime 0
instance Component WorldTime where type Storage WorldTime = Global WorldTime

newtype  Random = Random (R.StdGen)
instance Semigroup Random where (<>) = mappend
instance Monoid Random where mempty = Random $ R.mkStdGen 0 -- the IO portion of this is expected to initialize it with a seed
instance Component Random where type Storage Random = Global Random

newtype  Debug = Debug Bool
instance Semigroup Debug where (<>) = mappend
instance Monoid Debug where mempty = Debug False
instance Component Debug where type Storage Debug = Global Debug

-- Entity functions

stepLogicTime :: forall w m. (Has w m LogicTime, MonadIO m) => Word32 -> SystemT w m ()
stepLogicTime !dT = modify global $ \(LogicTime t) -> LogicTime (t + dT)

stepRenderTime :: forall w m. (Has w m RenderTime, MonadIO m) => Word32 -> SystemT w m ()
stepRenderTime !dT = modify global $ \(RenderTime t) -> RenderTime (t + dT)

stepWorldTime :: forall w m. (Has w m WorldTime, MonadIO m) => Word32 -> SystemT w m ()
stepWorldTime !dT = modify global $ \(WorldTime t) -> WorldTime (t + fromIntegral dT)