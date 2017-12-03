{-# LANGUAGE InstanceSigs #-}

module AST (Markdown(..)) where
import Test.QuickCheck
import Control.Applicative

data Markdown
  = Emphasis [Markdown]
  | StrongEmphasis [Markdown]
  | Italics Markdown
  | Link String Markdown
  | Image String Markdown
  | Header Int Markdown
  | Paragraph Markdown
  | OrderedList [Markdown]
  | UnorderedList [Markdown]
  | Text String
  | BlockQuote [Markdown]
  | BlockLiteral (Maybe String) String
  | InlineLiteral String
  | HorizontalRule
  deriving (Eq, Show)

instance Arbitrary Markdown where
  arbitrary :: Gen Markdown
  arbitrary = -- Block level only
    frequency
      [ (1, Paragraph <$> genInline) -- Is this correct? Not a list?
      , (1, OrderedList <$> genInlineList1)
      , (1, UnorderedList <$> genInlineList1)
      , (1, BlockQuote <$> genInlineList1)
      , (1, BlockLiteral <$> arbitrary <*> arbitrary)
      , (1, InlineLiteral <$> arbitrary)
      ]
    where
    genInline :: Gen Markdown
    genInline = frequency
      [ (10, Text <$> arbitrary)
      , (3, Emphasis <$> genInlineList1)
      , (3, StrongEmphasis <$> genInlineList1)
      ]
    genInlineList :: Gen [Markdown]
    genInlineList = frequency
      [ (3, return [])
      , (4, (:) <$> genInline <*> genInlineList)
      ]
    genInlineList1 :: Gen [Markdown]
    genInlineList1 = liftA2 (:) genInline genInlineList
  shrink :: Markdown -> [Markdown]
  shrink m = undefined