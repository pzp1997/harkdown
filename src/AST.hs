{-# LANGUAGE InstanceSigs #-}

module AST (Markdown(..)) where
import Test.QuickCheck
import Control.Applicative

data Markdown
  = Emphasis [Markdown]
  | StrongEmphasis [Markdown]
  | Italics [Markdown]
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
  shrink (Emphasis l)       = map Emphasis (shrink l)
  shrink (StrongEmphasis l) = map StrongEmphasis (shrink l)
  shrink (Italics l)        = map Italics (shrink l)
  -- For the following two, try both with shrunken link text and without
  shrink (Link s m)         =
    [Link s' m' | m' <- shrink m, s' <- shrink s] ++ map (Link s) (shrink m)
  shrink (Image s m)        =
    [Link s' m' | m' <- shrink m, s' <- shrink s] ++ map (Image s) (shrink m)
  -- For headers, try both with the same header level and with lower
  shrink (Header i m)       =
    [Header i' m' | i' <- shrink i, m' <- shrink m] ++ map (Header i) (shrink m)
  shrink (Paragraph m)      = map Paragraph $ shrink m
  shrink (OrderedList l)    = map OrderedList $ shrink l
  shrink (UnorderedList l)  = map UnorderedList $ shrink l
  shrink (Text s)           = map Text (shrink s)
  shrink (BlockQuote l)     = map BlockQuote $ shrink l
  shrink (BlockLiteral m s) = [BlockLiteral m' s' | m' <- shrink m, s' <- shrink s]
  shrink (InlineLiteral s)  = map InlineLiteral $ shrink s
  shrink (HorizontalRule)   = []
