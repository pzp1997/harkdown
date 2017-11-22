module InlineParser where

import Control.Applicative
import Control.Monad


import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)

import AST
import ParserCombinators

-- Useful parsers for testing if the next characters match something. None of
-- these parsers consume input.

-- | Consumes whitespace. Fails if the next character is not without consuming
--   input.
whitespace :: Parser Char
-- Note: Can't use isSpace because Haskell also includes \v. The standard does
-- not.
whitespace = try $ oneOf "\t\r\n\f"

-- | Consumes punctuation. Fails if it is not without consuming input.
punctuation :: Parser Char
punctuation = try $ oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- | Tests if the next data to consume is the provided string and meets the
--   criteria to be a left flanking delimiter. Consumes the delimiter on
--   success. Does not consume input on failure.
leftFlankingDelimiter :: String -> Parser String
leftFlankingDelimiter s = undefined

-- | Tests if the next data to consume is the provided string and meets the
--   criteria to be a right flanking delimiter. Consumes the delimiter on
--   success. Does not consume input on failure.
rightFlankingDelimiter :: String -> Parser String
rightFlankingDelimiter s = undefined

-- | Parses inline content and creates an inline AST. Stops at the end of
--   the block.
inline :: MdParser
inline = undefined

code :: MdParser
code = undefined

italics :: MdParser
italics = undefined

-- | Parser for an emphasis inline block. Emphasis is signified by another
--   inline block surrounded by a single * or _ character.
--   On failure the parser does not consume input.
--   This parser assumes that the calling parser has already checked that the
--   left flanking delimiter is not preceded by something to make it invalid.
--   Does not work with emphasis delimiter _. (Underscore is much more
--   complicated)
emphasis :: MdParser
emphasis = try parseStarEmphasis where
  parseStarEmphasis :: Parser IlParser
  parseStarEmphasis = leftFlankingDelimiter "*" *> inline <*
                      rightFlankingDelimiter "*"

-- | Parser for a strong emphasis inline block. Strong emphasis is signified
--   by two or three
strongEmphasis :: MdParser
strongEmphasis = undefined

link :: MdParser
link = undefined

image :: MdParser
image = undefined

autolink :: MdParser
autolink = undefined

text :: MdParser
text = undefined