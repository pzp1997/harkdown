module CMarkEquiv where

import Control.Applicative
import Data.Text (pack, unpack)

import Test.QuickCheck
import CMark (commonmarkToHtml)

import AST
import HtmlFormatter (renderHtml)
import Parser (runMainP)

cmarkTest :: Int -> IO ()
cmarkTest n = quickCheckWith (stdArgs { maxSuccess = n }) prop_cmark

prop_cmark :: [Markdown] -> Bool
prop_cmark doc = renderHtml (runMainP md) == unpack (commonmarkToHtml [] $ pack md)
  where md = show doc


instance Arbitrary Markdown where
  arbitrary = -- Block level only
    frequency
      [ (1, Paragraph <$> genInline)
      , (1, liftA3 OrderedList arbitrary arbitrary genInlineList1)
      , (1, liftA2 UnorderedList arbitrary genInlineList1)
      , (1, (BlockQuote . Many) <$> genInlineList1)
      , (1, CodeBlock <$> arbitrary <*> arbitrary)
      , (1, Code <$> arbitrary)
      ]
    where genInline = frequency [ (10, Text <$> arbitrary)
                                , (3, (Italics . Many) <$> genInlineList1)
                                , (3, (Bold . Many) <$> genInlineList1)
                                ]
          genInlineList = frequency [ (3, return [])
                                    , (4, liftA2 (:) genInline genInlineList)
                                    ]
          genInlineList1 :: Gen [Markdown]
          genInlineList1 = liftA2 (:) genInline genInlineList
  shrink (Bold l)            = Bold <$> shrink l
  shrink (Italics l)         = Italics <$> shrink l
  -- For the following two, try both with shrunken link text and without
  shrink (Link s _ m)        =
    [Link s' Nothing m' | m' <- shrink m, s' <- shrink s]
  shrink (Image s _ m)       =
    [Image s' Nothing m' | m' <- shrink m, s' <- shrink s]
  -- For headers, try both with the same header level and with lower
  shrink (Header i m)        =
    [Header i' m' | i' <- shrink i, m' <- shrink m] ++ (Header i <$> shrink m)
  shrink (Paragraph m)       = Paragraph <$> shrink m
  shrink (OrderedList i b l) = OrderedList i b <$> shrink l
  shrink (UnorderedList b l) = UnorderedList b <$> shrink l
  shrink (Text s)            = Text <$> shrink s
  shrink (BlockQuote l)      = BlockQuote <$> shrink l
  shrink (CodeBlock m s)     = [CodeBlock m' s' | m' <- shrink m, s' <- shrink s]
  shrink (Code s)            = Code <$> shrink s
  shrink _                   = []
