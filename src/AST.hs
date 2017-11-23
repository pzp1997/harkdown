module AST where

data Markdown
  = Bold Markdown
  | Italics Markdown
  | Link String Markdown
  | Image String Markdown
  | Header Int Markdown
  | Paragraph Markdown
  | OrderedList Int Bool [Markdown]
  | UnorderedList Bool [Markdown]
  | Text String
  | BlockQuote [Markdown]
  | CodeBlock (Maybe String) String
  | Code String
  | HorizontalRule
  | SoftBreak
  | HardBreak
  deriving (Eq, Show)

data Partial
  = PHeader Int String
  | POrderedList Int String
  | PUnorderedList String
  | PBlockQuote String
  | PCodeBlock (Maybe String) String
  | PHorizontalRule
  | PParagraph String
  | PBlankLine
  deriving (Eq, Show)

-- data Partial
--   = Content String
--   | PCodeBlock (Maybe String) String
--   | PThematic
--   | Atx Int String
--   | Setext Boolean
--   | OLItem Int String
--   | ULItem String
--   | BQItem String
--
--   | Content String
