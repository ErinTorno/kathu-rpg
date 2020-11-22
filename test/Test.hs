module Main where

import Test.HUnit

import Kathu.Test.Entity
import Kathu.Test.World

main :: IO ()
main = runTestTT tests >>= print
    where tests = TestList [entityTests, worldTests]