module Kathu.Editor.Tools.Info where

import           Apecs

-- Similar to Debug, but not meant to be toggled frequently
-- | Additional editor information should be added to entities when this is True
newtype  IncludeEditorInfo = IncludeEditorInfo {unEditorInfo :: Bool}
instance Semigroup IncludeEditorInfo where (<>) = mappend
instance Monoid IncludeEditorInfo where mempty  = IncludeEditorInfo False
instance Component IncludeEditorInfo where type Storage IncludeEditorInfo = Global IncludeEditorInfo