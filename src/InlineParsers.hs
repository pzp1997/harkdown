module InlineParser where

import Control.Applicative
import Control.Monad


import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)

import AST
import ParserCombinators

code :: MdParser
code = undefined

italics :: MdParser
italics = undefined

bold :: MdParser
bold = undefined

link :: MdParser
link = undefined

image :: MdParser
image = undefined

autolink :: MdParser
autolink = undefined

text :: MdParser
text = undefined