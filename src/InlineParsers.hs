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


import Test.HUnit

import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map

import AST
import ParserCombinators


-- Additional combinators --------------

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
  (lookAhead $ void (pwhitespace <|> plinebreak <|> ppunctuation) <|> eof)

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

-----------------------------------------

-- Second pass markdown parser (over tokens) ----

-- | Parser that works over the MdTokens defined above instead of Strings.
type TokenParser a = ParsecT [MdToken] () Identity a

-- | Utility that merges all text fields
simplify :: [Markdown] -> [Markdown]
simplify (Text "":xs)           = simplify xs
simplify (Text x:Text y:xs)     = simplify $ Text (x ++ y) : xs
simplify (Emphasis l:xs)        = Emphasis (simplify l) : simplify xs
simplify (StrongEmphasis l:xs)  = StrongEmphasis (simplify l) : simplify xs
simplify ((x@_):xs)             = x : simplify xs
simplify []                     = []

-- | Parser that recognizes a left flanking delimiter run of the supplied
--   character and using the supplied parser to recognize the delimiter. It
--   returns the pair of the maybe character consumed to delineate a left
--   flanking delimiter and the string used as the delimiter.
--   
--   If startOfDelimited is True, it's assumed that it is preceded by
--   punctuation or whitespace. Otherwise no such assumption can be made,
--   so such a token. must be directly consumed to be considered encountered.
leftFlankingDelimP :: Bool -> Char -> TokenParser String ->
    TokenParser (Maybe Char, String)
leftFlankingDelimP startOfDelimited c delim =
  if startOfDelimited
    then
      -- Only need to verify that it isn't followed by whitespace
      ((,) <$> pure Nothing <*> delim <*
        notFollowedBy (whitespaceParser <|> newLineParser))
    else do
      -- Must check for full rules.
      pre <- optionMaybe (whitespaceParser <|> punctParserN [c] <|> newLineParser)
      s <- delim -- delimiter
      -- (a) May not be followed by whitespace
      notFollowedBy (whitespaceParser <|> newLineParser)
      if isJust pre
        -- (b) Preceded by whitespace or punctuation
        then return (pre, s)
        -- (b) Not followed by punctuation
        else do
          notFollowedBy (punctParser)
          return (pre, s)
-- | Left flanking delimiter of a specified length and char
leftFlankingDelim :: Bool -> String -> TokenParser (Maybe Char, String)
leftFlankingDelim startOfDelimited delim =
  leftFlankingDelimP startOfDelimited (head delim) (punctParserSeq delim)
-- | Left flanking delimiter of maximal length using the supplied char
leftFlankingDelimAll :: Bool -> Char -> TokenParser (Maybe Char, String)
leftFlankingDelimAll startOfDelimited c =
  leftFlankingDelimP startOfDelimited c (many1 $ punctParserS [c])

-- A left-flanking delimiter run is a delimiter run that is (a) not followed by Unicode whitespace, and (b) not followed by a punctuation character, or preceded by Unicode whitespace or a punctuation character. For purposes of this definition, the beginning and the end of the line count as Unicode whitespace.

-- | Unit test
tleftFlankingDelimP :: Test
tleftFlankingDelimP = TestList
  [ "*hello" ~:
      runParser (leftFlankingDelim True "*") () "" [Punctuation '*',Word "hello"] ~?= Right (Nothing, "*")
  , "Token remains" ~:
      runParser (leftFlankingDelim True "*" *> text) () "" [Punctuation '*',Word "hello"] ~?= Right (Text "hello")
  , "Leading whitespace" ~:
      runParser (leftFlankingDelim False "*") () "" [Whitespace '\n',Punctuation '*',Word "hello"] ~?= Right (Just '\n', "*")
  ]

-- | Parses text and inline markdown until the matching right flanking
--   delimiter is encountered. It is parameterized by a boolean indicating
--   whether it's the first element in its enclosing context and a string,
--   boolean pair of the closing delimiter and whether an inline context just
--   closed, if a closing delimeter is expected.
textTillDelim :: Bool -> Maybe (String, Bool) -> TokenParser [Markdown]
textTillDelim isStartOfDelimited mEnd =
  simplify <$> do
  case mEnd of
    -- No end delimiter provided.
    Nothing ->
      -- Done
      (try eof *> pure []) <|>
      -- Inline content
      (try $ (++) <$> inlineContent isStartOfDelimited <*> textTillDelim False mEnd) <|>
      -- Consume another token as text and recurse
      (try $ (:) <$> text <*> textTillDelim False Nothing)

    -- end delimiter provided.
    Just (delim, prevJustClosed) ->
      -- First see if end has been reached.
      (try $ liftA (\x -> [Text x]) $ rightFlankingDelim delim prevJustClosed) <|>
      -- Inline content
      (try $ (++) <$> inlineContent isStartOfDelimited <*> textTillDelim False (Just (delim, True))) <|>
      -- Consume another token as text and recurse
      if prevJustClosed
        then -- Just recurse with the delimiter's boolean set to false
          textTillDelim isStartOfDelimited $ Just (delim, False)
        else -- Consume another token and retry
          (:) <$> text <*> textTillDelim False mEnd
  where
  -- | Parses a block of inline content, delimited by * or **. If the length of
  --   the delimiter is odd it first tries * and then **, and if even it tries
  --   in opposite order. It is parameterized by whether it is the first
  --   element in an enclosing context (so no previous tokens can exist).
  inlineContent :: Bool -> TokenParser [Markdown]
  inlineContent isStartOfDelimited = consumeInlineContent <$> do
    (a, maxdelim) <- lookAhead (try $ leftFlankingDelimAll isStartOfDelimited '*')
    let emphasis = emphasisP isStartOfDelimited "*"
        strongemphasis = emphasisP isStartOfDelimited "**"
    if even (length maxdelim)
      then try strongemphasis <|> emphasis
      else try emphasis <|> strongemphasis
    where
      consumeInlineContent :: (Maybe Char, Markdown) -> [Markdown]
      consumeInlineContent (Just c, m)  = [Text [c], m]
      consumeInlineContent (Nothing, m) = [m]

      emphasisP :: Bool -> String -> TokenParser (Maybe Char, Markdown)
      emphasisP isStartOfDelimited s = do
        (mC, delim) <- leftFlankingDelim isStartOfDelimited s
        -- To prevent the right flanking delimiter from matching immediately,
        -- we parameterize it with False. This forces the delimited section
        -- to include at least one token.
        content <- simplify <$> textTillDelim True (Just (delim, False))
        guard (not $ null content)
        return (mC, (if (length s == 1) then Emphasis else StrongEmphasis) content)

-- | Unit test
ttextTillDelim :: Test
ttextTillDelim = TestList
  [ "*hello world*" ~: runParser (textTillDelim True Nothing) () "" [Punctuation '*',Word "hello",Whitespace ' ',Word "world",Punctuation '*'] ~?= Right [Emphasis [Text "hello world"]]
  , "**hello world**" ~: runParser (textTillDelim True Nothing) () "" [Punctuation '*',Punctuation '*',Word "hello",Whitespace ' ',Word "world",Punctuation '*',Punctuation '*'] ~?= Right [StrongEmphasis [Text "hello world"]]
  , "***hello world***" ~: runParser (textTillDelim True Nothing) () "" [Punctuation '*',Punctuation '*',Punctuation '*',Word "hello",Whitespace ' ',Word "world",Punctuation '*',Punctuation '*',Punctuation '*'] ~?= Right [Emphasis [StrongEmphasis [Text "hello world"]]]
  , "***hello* world**" ~: runParser (textTillDelim True Nothing) () "" [Punctuation '*',Punctuation '*',Punctuation '*',Word "hello",Punctuation '*',Whitespace ' ',Word "world",Punctuation '*',Punctuation '*'] ~?= Right [StrongEmphasis [Emphasis [Text "hello"], Text " world"]]
  , "***hello** world*" ~: runParser (textTillDelim True Nothing) () "" [Punctuation '*',Punctuation '*',Punctuation '*',Word "hello",Punctuation '*',Punctuation '*',Whitespace ' ',Word "world",Punctuation '*'] ~?= Right [Emphasis [StrongEmphasis [Text "hello"], Text " world"]]
-- TODO disagreement. The js dingus says this next one should be
-- ***hello *<em>world</em>, but I think it should be <em>**hello **world</em>.
  , "***hello **world*" ~: runParser (textTillDelim True Nothing) () "" [Punctuation '*',Punctuation '*',Punctuation '*',Word "hello",Whitespace ' ',Punctuation '*',Punctuation '*',Word "world",Punctuation '*'] ~?= Right [Emphasis [Text "**hello **world"]]
  , "**hello** **world**" ~: runParser (textTillDelim True Nothing) () "" [Punctuation '*',Punctuation '*',Word "hello",Punctuation '*',Punctuation '*',Whitespace ' ',Punctuation '*',Punctuation '*',Word "world",Punctuation '*',Punctuation '*'] ~?= Right [StrongEmphasis [Text "hello"], Text " ",StrongEmphasis [Text "world"]]
  ]

-- | Parser that recognizes a right flanking delimiter run matching delim.
--   It returns the token that belongs to the content being delimited
--   that was consumed to find the delimiter (as the delimiter can't be
--   preceded by whitespace).
--   The delimiter is parameterized by a boolean, denoting whether the previous
--   delimiter just finished (so there won't be any tokens to consume)
-- TODO make it take (String,Bool) instead of taking them separately
rightFlankingDelim :: String -> Bool -> (TokenParser String)
rightFlankingDelim delim justFinishedPrev =
  if justFinishedPrev
    then
    -- If we just finished a delimited section, it must have had a punctuation
    -- character. Therefore we need only check that it is followed by whitespace
    -- or punctuation.
      (do
        punctParserSeq delim
        notFollowedBy (whitespaceParser <|> newLineParser <|> punctParser)
        return "") <|> rightFlankingDelim delim False
    else do
      -- General case -- TODO fix
      notFollowedBy (whitespaceParser <|> newLineParser)
      mPunct <- optionMaybe (punctParserN delim)
      case mPunct of
        Nothing -> do
          -- not preceded by punctuation
          -- Must be preceded by text.
          prev <- textString
          punctParserSeq delim
          return prev
        Just p  -> do
          -- preceded by punctuation p
          -- Must be followed by whitespace or punctuation
          punctParserSeq delim
          eof <|> void (lookAhead $ whitespaceParser <|> newLineParser <|> punctParser)
          return $ [p]

-- | Top level token parser for any kind of Markdown
inlineMarkdown :: TokenParser [Markdown]
inlineMarkdown = textTillDelim True Nothing


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

-- | Consumes a sequence of punctuation defined by the provided string. Fails
--   if it isn't matched (may consume input so should be used with try)
punctParserSeq :: String -> TokenParser String
punctParserSeq (c:cx) = (:) <$> punctParserS [c] <*> punctParserSeq cx
punctParserSeq []     = return []

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

-- | Parser that matches any whitespace. Must return unit to allow eof.
--   Doesn't match eof when using lookAhead.
anyWhitespaceParser :: TokenParser ()
anyWhitespaceParser = void whitespaceParser <|>
                      void newLineParser <|>
                      eof

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
  label <- optionMaybe anyTextString
  newLine
  content <- manyTill
    (anyTextString <|> textWhitespace)
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
text = Text <$> anyTextString

-- | Consumes any token and produces the contained value as a string. Should
--   generally be used in manyTill to consume until a stop condition is met.
anyTextString :: TokenParser String
anyTextString = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Whitespace  w -> Just [w]
    Punctuation p -> Just [p]
    Word        w -> Just w
    NewLine       -> Just "\n"

-- | Consumes a word token and produces the contained value as a string.
textString :: TokenParser String
textString = tokenPrim show nextPos testMatch
  where
  nextPos   ps x xs = incSourceColumn ps 1
  testMatch t       = case t of
    Word w -> Just w
    _      -> Nothing

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
