module AST where

type MdDoc = [Markdown]

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
  | BlockQuote [Markdown]
  | BlockLiteral (Maybe String) String
  | InlineLiteral String
  | HorizontalRule
  deriving (Eq, Show)
