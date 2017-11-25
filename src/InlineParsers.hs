{-# LANGUAGE InstanceSigs #-}

-- | Module to parse blocks of text into the Markdown AST.
module InlineParser (buildAST) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity


import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Prim
import Text.Parsec.Combinator

import Data.Maybe (listToMaybe)

import AST
import ParserCombinators

data MdToken
  = Whitespace Char
  | Punctuation Char
  | Word String
  deriving (Show, Eq)

-- | Parser that parses the maximal span of whitespace characters
pwhitespace :: Parser MdToken
pwhitespace = Whitespace <$> whitespace

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

-- Set up a parallel parser over the list so that it can be consumed
-- applicatively

type TokenParser a = ParsecT [MdToken] () Identity a

-- | Top level token parser for any kind of Markdown
inlineMarkdown :: TokenParser Markdown
inlineMarkdown = undefined


-- Utilities for parsing individual types.
-- | Consumes one punctuation token, where the punctuation character is in s.
--   Fails if the next token isn't punctuation or isn't the right character.
punctParser :: String -> TokenParser Char
punctParser s = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Punctuation c -> if c `elem` s then Just c else Nothing
    _             -> Nothing

-- | Consumes a block of whitespace, yielding the whitespace block encountered.
whitespaceParser :: TokenParser Char
whitespaceParser = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Whitespace w -> Just w
    _            -> Nothing

-- | Escapes punctuation characters. If a \ preceeds an escapable punctuation,
--   the following Punctuation type is replaced with a Word type and the \ is
--   discarded.
runEscapes :: [MdToken] -> [MdToken]
-- In the future we may want to pull this into the original tokenizer
runEscapes (Punctuation '\\':Punctuation x:xs)
  -- Escapable punctuation
  | x `elem` "*\\_[]<>()\"\'" = (Word [x]) : runEscapes xs
  -- Not escapable punctuation
  | otherwise = (Word "\\") : runEscapes (Punctuation x : xs)
runEscapes (x:xs) = x : runEscapes xs

-- | Consumes a star emphasis block, and generates a Bold Markdown block. The
--   contents of the block is itself a Markdown tree.
emphasisBlockStars :: TokenParser Markdown
emphasisBlockStars = Bold <$>
  (punctParser "*" *> manyTill (try inlineMarkdown) endParser)
  where
    endParser = do
      punctParser "*" -- Consume the closing *
      -- Whitespace must be there, but not consumed
      lookAhead $ try whitespaceParser
      return ()

-- | Consumes an underscore emphasis block, and generates a Bold Markdown
--   block. The contents of the block is itself a Markdown tree.
emphasisBlockUnderscores :: TokenParser Markdown
emphasisBlockUnderscores = Bold <$>
  (punctParser "_" *> manyTill (try inlineMarkdown) endParser)
  where
    endParser = do
      punctParser "_" -- Consume the closing *
      -- Whitespace must be there, but not consumed
      lookAhead $ try whitespaceParser
      return ()

-- | Parser for an emphasis block. Assumes that the calling parser already
--   ensured that the conditions for a left flanking delimiter have been met.
emphasisBlock :: TokenParser Markdown
emphasisBlock = emphasisBlockStars Control.Applicative.<|>
                emphasisBlockUnderscores

-- | Consumes an html pre, script, or style block and consumes all tokens until
--   it reaches its associated close tag. It then uses that to generate a Text
--   Markdown leaf.
preBlock :: TokenParser Markdown
preBlock = undefined

-- | Create a Markdown AST from the input string. Errors if the string can't be
--   fully consumed to create valid Markdown.

buildAST :: String -> Markdown
buildAST = undefined
{-
buildAST s = case tokenize s of
  Left err     -> error "Invalid markdown"
  Right tokens -> case listToMaybe $ dropNotFullyConsumed (doParse inlineMarkdown tokens) of
    Nothing     -> error "Invalid markdown"
    Just (a, _) -> a
  where
    dropNotFullyConsumed = dropWhile (\(_, l) -> not $ null l)
-}

-- | Parses inline content and creates an inline AST. Stops at the end of
--   the block.
inline :: TokenParser Markdown
inline = undefined

code :: TokenParser Markdown
code = undefined

italics :: TokenParser Markdown
italics = undefined

link :: TokenParser Markdown
link = undefined

image :: TokenParser Markdown
image = undefined

autolink :: TokenParser Markdown
autolink = undefined

text :: TokenParser Markdown
text = undefined