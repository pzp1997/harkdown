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
  | CodeBlock (Maybe String) String
  | Code String
  | HorizontalRule
  deriving (Eq, Show)

data Partial
  = PHeader Int String
  | PParagraph String
  | POrderedList [String]
  | PUnorderedList [String]
  | PBlockQuote String
  | PCodeBlock String
  | PHorizontalRule
  | PLinkRef String String
  deriving (Eq, Show)
