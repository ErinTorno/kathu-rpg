module Kathu.Random where

import           Apecs         hiding (Map)
import qualified System.Random as R

newtype  Random = Random R.StdGen
instance Semigroup Random where (<>) = mappend
instance Monoid Random where mempty  = Random $ R.mkStdGen 0 -- the IO portion of this is expected to initialize it with a seed
instance Component Random where type Storage Random = Global Random