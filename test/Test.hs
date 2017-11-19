module Main where

import Test.HUnit (Counts)
--
-- import HorizontalRule

import SpecTests

main :: IO Counts
main = runSpecTests
