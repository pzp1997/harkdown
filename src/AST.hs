module AST (Markdown(..)) where

data Markdown
  = Bold Markdown
  | Italics Markdown
  | Link String Markdown
  | Image String Markdown
  | Header Int Markdown
  | Paragraph Markdown
  | OrderedList [Markdown]
  | UnorderedList [Markdown]
  | Text String
  | BlockQuote Markdown
  | BlockLiteral String
  | InlineLiteral String
  | HorizontalRule
  deriving (Show)
