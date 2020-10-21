module Main where

import Test.HUnit

import Kathu.Test.Entity
import Kathu.Test.Util

main :: IO ()
main = runTestTT tests >>= print
    where tests = TestList [entityTests, utilTests]