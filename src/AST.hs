module AST where

import Data.Map (Map)

data Markdown
  = Bold Markdown
  | Italics Markdown
  | Link String (Maybe String) Markdown
  | Image String (Maybe String) Markdown
  | Header Int Markdown
  | Paragraph Markdown
  | OrderedList Int Bool [Markdown]
  | UnorderedList Bool [Markdown]
  | Text String
  | BlockQuote Markdown
  | CodeBlock String String
  | Code String
  | HorizontalRule
  | SoftBreak
  | HardBreak
  | Many [Markdown]
  deriving (Eq, Show)

data Partial
  = PHeader Int String
  | POrderedList Int Char Bool [[Partial]]
  | POrderedListItem Int Char String
  | PUnorderedList Char Bool [[Partial]]
  | PUnorderedListItem Char String
  | PBlockQuote [Partial]
  | PBlockQuoteItem String
  | PCodeBlock String String
  | PHorizontalRule
  | PParagraph String
  | PBlankLine
  | PLinkRef String String
  deriving (Eq, Show)

type LinkRefMap = Map String String
