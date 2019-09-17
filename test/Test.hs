module Main where

import Test.HUnit

import EntityTests
import GraphicsTests

main :: IO ()
main = runTestTT tests >>= print
    where tests = TestList [entityTests, graphicsTests]