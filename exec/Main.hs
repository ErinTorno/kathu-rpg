module Main where

import           System.Environment

import qualified Kathu.App          as Kathu

main :: IO ()
main = getArgs >>= Kathu.start