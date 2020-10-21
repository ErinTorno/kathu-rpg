module Main where

import Test.HUnit

import Verda.Test.Graphics.Color

main :: IO ()
main = print =<< runTestTT tests
    where tests = TestList [colorTests]