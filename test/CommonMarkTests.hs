
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- Test code that loads in the json located at test/spec.json, converts them
-- into test cases, and runs them.
module CommonMarkTests.Main(main) where

import Test.HUnit
import qualified Data.ByteString.Lazy as B

import GHC.Generics
import Data.Aeson

import Harkdown.Converter

data Spec = Spec {
         end_line :: Int
       , section :: String
       , html :: String
       , markdown :: String
       , exampleNum :: Int
       , start_line :: Int
       } deriving (Generic, Show)

instance FromJSON Spec

-- | The spec file to use
specFile :: FilePath
specFile = "spec.json"

-- | Get the bytestring from the spec file source
getJSON :: IO B.ByteString
getJSON = B.readFile specFile

-- | Convert a single spec record into a HUnit Test
specToTest :: Spec -> Test
specToTest s =
  (show (exampleNum s) ++
  ". " ++
  section s ++
  ": " ++
  show (start_line s) ++
  "-" ++
  show (end_line s)) ~: convertToHTML (markdown s) ~?= html s

-- | Run tests
main :: IO Counts
main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Spec])
  case d of
    Left err    -> do
      runTestTT $ TestCase (assertBool err False)
    Right specs -> do
      runTestTT $ TestList (fmap specToTest specs)