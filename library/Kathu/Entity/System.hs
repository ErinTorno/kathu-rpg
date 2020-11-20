{-# OPTIONS_GHC -fno-warn-orphans #-}
-- we need orphan instances to set up the Apecs system

module Kathu.Entity.System where

import           Apecs hiding (Map)
import qualified System.Random as R

-- New Globals

newtype  Random = Random R.StdGen
instance Semigroup Random where (<>) = mappend
instance Monoid Random where mempty  = Random $ R.mkStdGen 0 -- the IO portion of this is expected to initialize it with a seed
instance Component Random where type Storage Random = Global Random

---------------------------
-- Debug-related globals --
---------------------------

-- Similar to Debug, but not meant to be toggled frequently
-- | Additional editor information should be added to entities when this is True
newtype  IncludeEditorInfo = IncludeEditorInfo {unEditorInfo :: Bool}
instance Semigroup IncludeEditorInfo where (<>) = mappend
instance Monoid IncludeEditorInfo where mempty  = IncludeEditorInfo False
instance Component IncludeEditorInfo where type Storage IncludeEditorInfo = Global IncludeEditorInfo

-- Entity functions