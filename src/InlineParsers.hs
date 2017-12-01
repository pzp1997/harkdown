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

import Data.Maybe (isJust)

import AST
import ParserCombinators


-- Additional combinators --------------

-- | Utility to throw away the result of the parser. Used to make combinators happy.
skip :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
skip p = do
  p
  return ()

-- | Variant of manyTill that must match at least once.
many1Till :: Stream s m t =>
  ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p end = do
  res <- manyTill p end
  guard (not $ null res)
  return res

-----------------------------------------

-- Initial pass tokenization ------------

-- | Data structure used for the tokenization of the input
data MdToken
  = Whitespace Char
  | NewLine
  | Punctuation Char
  | Word String
  | StartOfFile
  deriving (Show, Eq)

-- | Parser that parses the maximal span of whitespace characters
pwhitespace :: Parser MdToken
pwhitespace = Whitespace <$> whitespace

-- | Consumes whitespace.
whitespace :: Parser Char
-- All characters considered whitespace by Markdown excluding newlines.
whitespace = oneOf "\t\f "

-- | Parses any form of a newline [\r, \r\n, \n] and returns a NewLine.
plinebreak :: Parser MdToken
plinebreak = do
  -- needs to be in try so that the leading \r isn't consumed on failure.
  (try endOfLine) <|> char '\r'
  return $ NewLine

-- | Parser that parses a single punctuation character.
ppunctuation :: Parser MdToken
ppunctuation = Punctuation <$> punctuation

-- | Consumes punctuation.
punctuation :: Parser Char
-- All characters considered punctuation by Markdown.
punctuation = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- | Parser that parses the maximal span of characters that aren't whitespace
--   or punctuation.
pword :: Parser MdToken
pword = Word <$> many1Till
  anyChar
  (lookAhead $ skip (pwhitespace <|> plinebreak <|> ppunctuation) <|> eof)

tokenizer :: Parser [MdToken]
tokenizer = do
  tokens <- Prim.many $ choice
    [ ppunctuation
    , plinebreak
    , pwhitespace
    , pword
    ]
  return (StartOfFile : tokens)

-- | Runs the tokenizer on the provided input.
tokenize :: String -> Either ParseError [MdToken]
tokenize = runParser tokenizer () ""

-----------------------------------------

-- Second pass markdown parser (over tokens) ----

-- | Parser that works over the MdTokens defined above instead of Strings.
type TokenParser a = ParsecT [MdToken] () Identity a

-- | Parser that recognizes a left flanking delimiter run of the supplied
--   character. It returns the pair of the maybe character consumed to
--   delineate a left flanking delimiter and the string used as the delimiter.
leftFlankingDelimAll :: Char -> TokenParser (Maybe Char, String)
leftFlankingDelimAll c = do
  pre <- optionMaybe (whitespaceParser <|> punctParserN [c] <|> newLineParser)
  sof <- optionMaybe startOfFileParser
  s <- many1 $ punctParserS [c]-- delimiter
  notFollowedBy (whitespaceParser <|> newLineParser)
  if isJust pre || isJust sof
    then return (pre, s)
    else do
      notFollowedBy punctParser
      return (pre, s)

-- | Parser that recognizes a left flanking delimiter run of the supplied
--   character. It returns the pair of the maybe char consumed to
--   delineate a left flanking delimiter (whitespace or punctuation) and the
--   string used as the delimiter.
leftFlankingDelimLen :: Int -> Char -> TokenParser (Maybe Char, String)
leftFlankingDelimLen length c = do
  pre <- optionMaybe (whitespaceParser <|> punctParserN [c] <|> newLineParser)
  sof <- optionMaybe startOfFileParser
  s <- count length $ punctParserS [c]-- delimiter
  notFollowedBy (whitespaceParser <|> newLineParser)
  if isJust pre || isJust sof
    then return (pre, s)
    else do
      notFollowedBy punctParser
      return (pre, s)

-- | Parses as much as possible until it encounters any valid left flanking
--   delimiter or the provided specific right flanking delimiter. If a left
--   flanking delimiter is encountered it attempts to recurse for that nested
--   markdown document. On success or failure of the recursed parser it then
--   resumes looking for the matching right flanking delimiter specified. If no
--   right flanking delimiter is provided it consumes until the end of file.
textTillDelim :: Maybe (TokenParser String) -> TokenParser [Markdown]
textTillDelim mEnd = do
  done <- optionMaybe eof
  case done of
    Just _  -> return [] -- EOF reached
    Nothing -> do
      -- If an end delimiter was provided, attempt to apply it.
      case mEnd of
        Nothing -> leftFlankOrText
        Just end -> do
          endFound <- optionMaybe (try end)
          case endFound of
            Just _  ->
              -- Found the end!
              return []
            Nothing -> leftFlankOrText
  where
  leftFlankOrText = do
    delimRes <- optionMaybe (try $ leftFlankingDelimLen 1 '*')
    
    case delimRes of
      Nothing             ->
        -- No left flanking delimiter. Consume another token as text.
        (:) <$> text <*> textTillDelim Nothing
      Just (mC, delim) -> do -- Left flanking delimiter found
        -- Content that will go into the delimited element
        inline <- textTillDelim (Just $ rightFlankingDelim (length delim) (head delim))
        -- Everything after
        rem    <- textTillDelim Nothing
        case mC of
          Nothing -> do
            -- Don't need to consume anything extra
            return $ (Emphasis inline) : rem
          Just c -> do
            -- Need to consume the extra token (whitespace, newline, punct)
            return $ (Text [c]) : (Emphasis inline) : rem

-- | Parser that recognizes a right flanking delimiter run of the supplied
--   length using the supplied character. It returns the string used as the
--   delimiter.
rightFlankingDelim :: Int -> Char -> TokenParser String
rightFlankingDelim length c = do
  notFollowedBy (anyWhitespaceParser)
  pre <- optionMaybe (punctParserN [c])
  s <- count length $ punctParserS [c]
  if isJust pre
    then return s
    else do
      -- Must be followed by one of these
      lookAhead (anyWhitespaceParser <|> skip (punctParserN [c]))
      return s

-- | Top level token parser for any kind of Markdown
inlineMarkdown :: TokenParser [Markdown]
inlineMarkdown = textTillDelim Nothing


-- Utilities for parsing individual types.
-- | Consumes one punctuation token. Fails if the next token isn't punctuation.
punctParser :: TokenParser Char
punctParser = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Punctuation c -> Just c
    _             -> Nothing

-- | Consumes one punctuation token, where the punctuation character is in s.
--   Fails if the next token isn't punctuation or isn't the right character.
punctParserS :: String -> TokenParser Char
punctParserS s = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Punctuation c -> if c `elem` s then Just c else Nothing
    _             -> Nothing

-- | Consumes one punctuation token unless the punctuation character is in s.
--   Fails if the next token isn't punctuation or is in the exception list.
punctParserN :: String -> TokenParser Char
punctParserN except = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Punctuation c -> if c `elem` except then Nothing else Just c
    _             -> Nothing

-- | Consumes a block of whitespace, yielding the whitespace character
--   encountered.
whitespaceParser :: TokenParser Char
whitespaceParser = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Whitespace w -> Just w
    _            -> Nothing

-- | Consumes a newline, yielding the \n character
newLineParser :: TokenParser Char
newLineParser = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    NewLine -> Just '\n'
    _       -> Nothing

-- | Parser that matches any whitespace. Must return unit to allow eof or
--   StartOfFile.
anyWhitespaceParser :: TokenParser ()
anyWhitespaceParser = skip whitespaceParser <|>
                      skip newLineParser <|>
                      eof <|>
                      startOfFileParser

startOfFileParser :: TokenParser ()
startOfFileParser = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    StartOfFile -> Just ()
    _           -> Nothing

-- | Escapes punctuation characters. If a \ preceeds an escapable punctuation,
--   the following Punctuation type is replaced with a Word type and the \ is
--   discarded.
runEscapes :: [MdToken] -> [MdToken]
-- In the future we may want to pull this into the original tokenizer
runEscapes (Punctuation '\\' : (Punctuation x) : xs)
  -- Escapable punctuation
  | x `elem` "*\\_[]<>()\"\'" = (Word [x]) : runEscapes xs
  -- Not escapable punctuation
  | otherwise                 = (Word "\\") : runEscapes (Punctuation x : xs)
runEscapes (x:xs) = x : runEscapes xs
runEscapes []     = []

-- | Version of count that throws away the contents
count_ :: Int -> TokenParser a -> TokenParser ()
count_ x p = do
  count x p
  return ()

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
  C.optional $ Prim.many textWhitespace
  atLeast_ 3 (punctParserS "`")
  -- Find out how many additional ticks the user gave
  addlTicks <- many (punctParserS "`")
  label <- optionMaybe textString
  newLine
  content <- manyTill
    (textString <|> textWhitespace)
    ((try $ count_ (3 + length addlTicks) (punctParserS "`")) <|> eof)
  -- Throw away whitespace on the same line
  C.optional $ Prim.many textWhitespace
  C.optional newLine
  return $ BlockLiteral label (foldr (++) "" content)

italics :: TokenParser Markdown
italics = undefined

link :: TokenParser Markdown
link = undefined

image :: TokenParser Markdown
image = undefined

autolink :: TokenParser Markdown
autolink = undefined

-- | Parser that consumes any token as text. Should only be used after all
--   other possibilities have been exhausted.
--   
text :: TokenParser Markdown
text = Text <$> textString

-- | Consumes any token and produces the contained value as a string. Should
--   generally be used in manyTill to consume until a stop condition is met.
textString :: TokenParser String
textString = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Whitespace  w -> Just [w]
    Punctuation p -> Just [p]
    Word        w -> Just w
    NewLine       -> Just "\n"
    StartOfFile   -> Just ""

textWhitespace :: TokenParser String
textWhitespace = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Whitespace  w -> Just [w]
    _             -> Nothing

newLine :: TokenParser Char
newLine = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    NewLine -> Just '\n'
    _       -> Nothing
