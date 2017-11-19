module Main where

import Test.HUnit (Counts)
--
-- import HorizontalRule

import SpecTests
import CMarkTest (cmarkTest)

main :: IO Counts
main = do cmarkTest 500
          runSpecTests
