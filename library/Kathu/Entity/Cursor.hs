module Kathu.Entity.Cursor where

import           Apecs
import           Data.Int
import           Linear.V2 (V2(..))

-- | Holds information about the cursor (usually mouse)
data CursorMotionState = CursorMotionState
    { cursorScreenPosition :: !(V2 Int32)
    , cursorPosition       :: !(V2 Double)
    , cursorMovement       :: !(V2 Double)
    , cursorScrollWheel    :: !Double      -- if scrolling isn't supported, should always be 0
    } deriving (Show, Eq)

instance Semigroup CursorMotionState where (<>) = mappend
instance Monoid CursorMotionState where mempty = CursorMotionState (V2 0 0) (V2 0 0) (V2 0 0) 0
instance Component CursorMotionState where type Storage CursorMotionState = Global CursorMotionState