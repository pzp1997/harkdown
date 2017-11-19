module CMarkTest where

import CMark (commonmarkToHtml)
import Data.Text (pack, unpack)
import Test.QuickCheck

import AST
import HtmlFormatter (markdownToHtml)

cmarkTest :: Int -> IO ()
cmarkTest n = quickCheckWith (stdArgs { maxSuccess = n }) prop_cmark

prop_cmark :: MdDoc -> Bool
prop_cmark doc = markdownToHtml md == unpack (commonmarkToHtml [] $ pack md)
  where md = show doc

instance Arbitrary Markdown where
  arbitrary = undefined -- TODO
