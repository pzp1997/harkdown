module SpecTests where

import Data.Aeson
import qualified Data.ByteString.Lazy as ByteS (readFile)
import Data.List (groupBy)
import Data.Text (Text, pack)
import Test.HUnit

import HtmlFormatter (markdownToHtml)

runSpecTests :: IO Counts
runSpecTests = do
  specJson <- ByteS.readFile "test/spec.json"
  case eitherDecode specJson of
    Right examples -> runTestTT $ testSuite examples
    Left errMsg    -> runTestTT $ TestCase $ assertBool errMsg False

data Example = Example
  { exampleSection  :: String
  , exampleHtml     :: String
  , exampleMarkdown :: String
  , exampleId       :: Int
  }

sectionText, htmlText, markdownText, exampleText :: Text
sectionText  = pack "section"
htmlText     = pack "html"
markdownText = pack "markdown"
exampleText  = pack "example"

instance FromJSON Example where
  parseJSON = withObject "test case" $ \o ->
    Example <$> o .: sectionText
            <*> o .: htmlText
            <*> o .: markdownText
            <*> o .: exampleText

testExample :: Example -> Test
testExample example = show (exampleId example) ~:
  markdownToHtml (exampleMarkdown example) ~?= exampleHtml example

testSection :: [Example] -> Test
testSection []                     = TestList []
testSection examples@(example : _) = exampleSection example ~:
  TestList $ testExample <$> examples

testSuite :: [Example] -> Test
testSuite = TestList . fmap testSection . groupBy sameSection
  where sameSection a b = exampleSection a == exampleSection b
