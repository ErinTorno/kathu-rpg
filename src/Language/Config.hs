module Language.Config where

import Data.Text (Text)
import IO.Path

data Language = Language
    { langName :: Text
    , appName :: Text
    -- , langIcon :: Drawable
    , fontDir :: FileDescriptor
    , failLine :: Text
    --, lines :: LineTree
    }

--data Line = Line {lnText :: Text, lnForms :: Map Text Text, lnTags :: [Text]}