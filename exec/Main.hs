module Main where

import           System.Environment

import qualified Kathu.App.Main     as Kathu
import qualified Kathu.Editor.Main  as Editor

main :: IO ()
main = do
    args <- getArgs
    if   Editor.shouldRunEditor args
    then Editor.start args
    else Kathu.start