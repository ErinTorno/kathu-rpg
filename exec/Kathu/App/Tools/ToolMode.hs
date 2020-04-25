{-# LANGUAGE TypeFamilies #-}

module Kathu.App.Tools.ToolMode where

import Apecs

-- | A global component that determines whether normal camera rules are overriden and a mouse-based movie should be used
data ToolMode = NoTool | TilePlacer deriving (Show, Eq)

instance Semigroup ToolMode where (<>) = mappend
instance Monoid ToolMode where mempty = NoTool
instance Component ToolMode where type Storage ToolMode = Global ToolMode

usesFreeCam :: ToolMode -> Bool
usesFreeCam NoTool = False
usesFreeCam _      = True