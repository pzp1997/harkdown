module AST where

data Markdown
  = Bold [Markdown]
  | Italics [Markdown]
  | Link String (Maybe String) Markdown
  | Image String (Maybe String) Markdown
  | Header Int Markdown
  | Paragraph Markdown
  | OrderedList Int Bool [[Markdown]]
  | UnorderedList Bool [[Markdown]]
  | Text String
  | BlockQuote [Markdown]
  | CodeBlock String String
  | Code String
  | HorizontalRule
  | SoftBreak
  | HardBreak
  deriving (Eq, Show)

data Partial
  = PHeader Int String
  | POrderedListItem Int Char String
  | PUnorderedListItem Char String
  | POrderedList Int Char Bool [[Markdown]]
  | PUnorderedList Char Bool [[Markdown]]
  | PBlockQuote String
  | PCodeBlock String String
  | PHorizontalRule
  | PParagraph String
  | PBlankLine
  deriving (Eq, Show)
