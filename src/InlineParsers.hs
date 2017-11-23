-- | Module to parse blocks of text into the Markdown AST.
module InlineParser (buildAST) where

import Control.Applicative
import Control.Monad


import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)

import AST
import ParserCombinators

data MdToken
  = Whitespace String
  | Punctuation Char
  | Word String
  deriving (Show, Eq)

-- | Parser that parses the maximal span of whitespace characters
pwhitespace :: Parser MdToken
pwhitespace = Whitespace <$> many1 whitespace

-- | Parser that parses a single punctuation character.
ppunctuation :: Parser MdToken
ppunctuation = Punctuation <$> punctuation

-- | Parser that parses the maximal span of characters that aren't whitespace
--   or punctuation
pword :: Parser MdToken
pword = Word <$> many1 (noneOf $ punctuationchars ++ whitespacechars) where

tokenizer :: Parser [MdToken]
tokenizer = do
  tokens <- manyTill (choice
    [ ppunctuation
    , pwhitespace
    , pword
    ]) eof
  return tokens

-- | Runs the tokenizer on the provided input.
tokenize :: String -> Either ParseError [MdToken]
tokenize = runParser tokenizer () ""

-- | Consumes whitespace. Fails if the next character is not without consuming
--   input.
whitespace :: Parser Char
whitespace = try (oneOf whitespacechars)

-- | All characters considered whitespace by Markdown.
-- Note: Can't use isSpace because Haskell also includes \v. The standard does
-- not.
whitespacechars :: String
whitespacechars = "\t\r\n\f "

-- | Consumes punctuation. Fails if it is not without consuming input.
punctuation :: Parser Char
punctuation = try $ oneOf punctuationchars

-- | All characters considered punctuation by Markdown.
punctuationchars :: String
punctuationchars = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- | Create a Markdown AST from the input string
buildAST :: String -> Markdown
buildAST s = case tokenize s of
  Left err     -> error "Invalid markdown"
  Right tokens -> undefined

{-
-- | Tests if the next data to consume is a sequence of the provided character
--   and that it meets the criteria to be a left flanking delimiter. Consumes
--   the delimiter on success. Does not consume input on failure.
leftFlankingDelimiter :: Char -> Parser String
leftFlankingDelimiter c = try match where
  match = do
    delim <- many1 c
    notFollowedBy whitespace
    notFollowedBy punctuation <|> (notFollowedBy $ string ['\\', c])
    return delim

-- | Tests if the next data to consume is the provided string and meets the
--   criteria to be a right flanking delimiter. Consumes the delimiter on
--   success. Does not consume input on failure.
rightFlankingDelimiter :: String -> Parser String
rightFlankingDelimiter s = try match where
  match = undefined
   -}

-- | Parses inline content and creates an inline AST. Stops at the end of
--   the block.
inline :: Parser Markdown
inline = undefined

code :: Parser Markdown
code = undefined

italics :: Parser Markdown
italics = undefined
{-
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
-}
link :: Parser Markdown
link = undefined

image :: Parser Markdown
image = undefined

autolink :: Parser Markdown
autolink = undefined

text :: Parser Markdown
text = undefined