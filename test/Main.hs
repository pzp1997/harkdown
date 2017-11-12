module Main where

import Test.HUnit (Counts, Test(TestList), runTestTT)

import HorizontalRule

main :: IO Counts
main = runTestTT $ TestList
  [ tHorizontalRule ]
