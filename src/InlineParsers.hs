{-# LANGUAGE InstanceSigs #-}

-- | Module to parse blocks of text into the Markdown AST.
module InlineParser (buildAST) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity


import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Prim as Prim (many)
import qualified Text.Parsec.Combinator as C

import Data.Maybe (listToMaybe)

import AST
import ParserCombinators

-- | Data structure used for the tokenization of the input
data MdToken
  = Whitespace Char
  | Punctuation Char
  | Word String
  deriving (Show, Eq)

-- | Parser that parses the maximal span of whitespace characters
pwhitespace :: Parser MdToken
pwhitespace = Whitespace <$> whitespace

-- | Parses any form of a newline [\r, \r\n, \n] and returns \n as whitespace.
--   Fails if it can't match the appropriate whitespace.
plinebreak :: Parser MdToken
plinebreak = do
  char '\r' <|> endOfLine
  return $ Whitespace '\n'

-- | Parser that parses a single punctuation character.
ppunctuation :: Parser MdToken
ppunctuation = Punctuation <$> punctuation

-- | Parser that parses the maximal span of characters that aren't whitespace
--   or punctuation
pword :: Parser MdToken
pword = Word <$> many1 (noneOf $ punctuationchars ++ whitespacechars) where

tokenizer :: Parser [MdToken]
tokenizer = do
  tokens <- Prim.many $ choice
    [ ppunctuation
    , plinebreak
    , pwhitespace
    , pword
    ]
  return tokens

-- | Runs the tokenizer on the provided input.
tokenize :: String -> Either ParseError [MdToken]
tokenize = runParser tokenizer () ""

-- | Consumes whitespace. Fails if the next character is not without consuming
--   input.
whitespace :: Parser Char
whitespace = try (oneOf whitespacechars)

-- | All characters considered whitespace by Markdown excluding newlines.
whitespacechars :: String
whitespacechars = "\t\f "

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
inlineMarkdown :: TokenParser [Markdown]
inlineMarkdown = Prim.many $ choice
    [ emphasisBlock
    , italics
    , code
    , preBlock
    , link
    , image
    , autolink
    , text
    ]


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

-- | Version of count that throws away the contents
count_ :: Int -> TokenParser a -> TokenParser ()
count_ x p = do
  count x p
  return ()

-- | Consumes a star emphasis block, and generates a Bold Markdown block. The
--   contents of the block is itself a Markdown tree.
emphasisBlockStars :: TokenParser Markdown
emphasisBlockStars = Bold <$>
  (punctParser "*" *> (try inlineMarkdown) <* endParser)
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
  (punctParser "_" *> (try inlineMarkdown) <* endParser)
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

buildAST :: String -> [Markdown]
buildAST s = case do
  tokens <- tokenize s
  runParser inlineMarkdown () "" (runEscapes tokens) of
  (Right result) -> result
  (Left err)     -> error $ show err

code :: TokenParser Markdown
code = try $ do
  C.optional $ Prim.many whitespaceNoNewline
  count 3 (punctParser "`")
  label <- optionMaybe textString
  C.optional $ Prim.many textWhitespace
  content <- manyTill
    (textString Control.Applicative.<|> textWhitespace)
    ((try $ count_ 3 (punctParser "`")) <|> eof)
  -- Throw away whitespace on the same line
  C.optional $ Prim.many whitespaceNoNewline
  return $ BlockLiteral label (foldr (++) "" content)

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

-- | Consumes any token and produces the contained value as a string. Should generally be used in manyTill
--   to consume until a stop condition is met.
textString :: TokenParser String
textString = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Whitespace  w -> Just [w]
    Punctuation p -> Just [p]
    Word        w -> Just w

textWhitespace :: TokenParser String
textWhitespace = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Whitespace  w -> Just [w]
    _             -> Nothing

-- Parses any whitespace tokens that are not newlines and creates Text.
whitespaceNoNewline :: TokenParser String
whitespaceNoNewline = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Whitespace '\n' -> Nothing
    Whitespace '\r' -> Nothing
    Whitespace  w   -> Just [w]
    _               -> Nothing