module Main where

import Test.HUnit

import EntityTests
import GraphicsTests
import PhysicsTests

main :: IO ()
main = runTestTT tests >>= print
    where tests = TestList [entityTests, graphicsTests, physicsTests]