module Kathu.Editor.File where

import           Apecs                      hiding (set)
import           Control.Lens

import           Kathu.App.Data.Library
import           Kathu.App.Graphics.Image   (ImageID)
import           Kathu.App.Tools.EventQueue
import           Kathu.IO.File              (loadFromFileDP)
import           Kathu.Util.Dependency
import           Kathu.World.WorldSpace

loadWorldSpace :: EventQueue -> FilePath -> IO (WorldSpace ImageID)
loadWorldSpace queue file = runWithEntityWorld queue $ do
    library  <- get global
    worldDep <- lift $ loadFromFileDP file
    (worldspace, store') <- runDependency worldDep (library^.kathuStore)
    global   $= set kathuStore store' library
    pure worldspace