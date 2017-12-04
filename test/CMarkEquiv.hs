module CMarkEquiv where

import Control.Applicative

import Test.QuickCheck

import AST

instance Arbitrary Markdown where
  arbitrary = -- Block level only
    frequency
      [ (1, Paragraph <$> genInline) -- Is this correct? Not a list?
      , (1, OrderedList <$> genInlineList1)
      , (1, UnorderedList <$> genInlineList1)
      , (1, BlockQuote <$> genInlineList1)
      , (1, CodeBlock <$> arbitrary <*> arbitrary)
      , (1, InlineLiteral <$> arbitrary)
      ]
    where genInline = frequency [ (10, Text <$> arbitrary)
                                , (3, Italics <$> genInlineList1)
                                , (3, Bold <$> genInlineList1)
                                ]
          genInlineList = frequency [ (3, return [])
                                    , (4, liftA2 (:) genInline genInlineList)
                                    ]
          genInlineList1 = liftA2 (:) genInline genInlineList
  shrink (Italics l)       = Italics <$> shrink l
  shrink (Bold l) = Bold <$> shrink l
  shrink (Italics l)        = Italics <$> shrink l
  -- For the following two, try both with shrunken link text and without
  shrink (Link s m)         =
    [Link s' m' | m' <- shrink m, s' <- shrink s] ++ (Link s <$> shrink m)
  shrink (Image s m)        =
    [Link s' m' | m' <- shrink m, s' <- shrink s] ++ (Image s <$> shrink m)
  -- For headers, try both with the same header level and with lower
  shrink (Header i m)       =
    [Header i' m' | i' <- shrink i, m' <- shrink m] ++ (Header i <$> shrink m)
  shrink (Paragraph m)      = Paragraph <$> shrink m
  shrink (OrderedList l)    = OrderedList <$> shrink l
  shrink (UnorderedList l)  = UnorderedList <$> shrink l
  shrink (Text s)           = Text <$> shrink s
  shrink (BlockQuote l)     = BlockQuote <$> shrink l
  shrink (CodeBlock m s) = [CodeBlock m' s' | m' <- shrink m, s' <- shrink s]
  shrink (InlineLiteral s)  = InlineLiteral <$> shrink s
  shrink HorizontalRule     = []
