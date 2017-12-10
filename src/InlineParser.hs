module InlineParser where

import Control.Applicative
import Control.Monad
import Data.Maybe (isJust)

import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)

import AST
import ParserCombinators

-- Imports for tests only
import Test.HUnit
import qualified Data.Map.Strict as Map

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
pwhitespace = Whitespace <$> oneOf "\t\f "

-- | Parses any form of a newline [\r, \r\n, \n] and returns a NewLine.
plinebreak :: Parser MdToken
plinebreak = eol *> pure NewLine

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
pword = Word <$> someTill anyChar
  (lookAhead $ void (pwhitespace <|> plinebreak <|> ppunctuation) <|> eof)

tokenizer :: Parser [MdToken]
tokenizer = many $ choice
    [ ppunctuation
    , plinebreak
    , pwhitespace
    , pword
    ]

-- | Runs the tokenizer on the provided input.
tokenize :: String -> Either ParseError [MdToken]
tokenize = runParser tokenizer () ""

-----------------------------------------

-- Second pass markdown parser (over tokens) ----

-- | Parser that works over the MdTokens defined above instead of Strings.
type TokenParser a = Parsec [MdToken] LinkRefMap a

-- | Utility that merges all text fields
simplify :: [Markdown] -> [Markdown]
simplify (Many xs : Many ys : rest) = simplify $ Many (xs ++ ys) : rest
simplify (Many [] : rest)           = simplify rest
simplify (Many [md] : rest)         = simplify $ md : rest
simplify (Many xs : rest)           = Many (simplify xs) : simplify rest
simplify (Text s : Text t : rest)   = simplify $ Text (s ++ t) : rest
simplify (Text "" : rest)           = simplify rest
simplify (Italics md : xs)          = Italics (Many $ simplify [md]) : simplify xs
simplify (Bold md : xs)             = Bold (Many $ simplify [md]) : simplify xs
simplify (x : xs)                   = x : simplify xs
simplify []                         = []


-- TODO add type
-- interrupt = choice
--   [ pure <$> (whitespaceParser <|> punctParserN "*") <* lookAhead (some (punctParserS "*") *> betterNotFollowedBy whitespaceParser)
--   , "" <$ lookAhead (some (punctParserS "*") *> betterNotFollowedBy (whitespaceParser <|> punctParser))
--   ]


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
      (,) <$> pure Nothing <*> delim <*
        notFollowedBy (whitespaceParser <|> newLineParser)
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
          notFollowedBy punctParser
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
      runParser (leftFlankingDelim True "*") Map.empty "" [Punctuation '*',Word "hello"] ~?= Right (Nothing, "*")
  , "Token remains" ~:
      runParser (leftFlankingDelim True "*" *> text) Map.empty "" [Punctuation '*',Word "hello"] ~?= Right (Text "hello")
  , "Leading whitespace" ~:
      runParser (leftFlankingDelim False "*") Map.empty "" [Whitespace '\n',Punctuation '*',Word "hello"] ~?= Right (Just '\n', "*")
  ]



-- | Parses inline markdown and text until the matching right flanking
--   delimiter is encountered. It is parameterized by a boolean indicating
--   whether it's the first element in its enclosing context and a string,
--   boolean pair of the closing delimiter and whether an inline context just
--   closed.
mdTillDelim :: Bool -> (String, Bool) -> TokenParser [Markdown]
mdTillDelim isStartOfDelimited (delim, prevJustClosed) =
  simplify <$>
    -- First see if end has been reached.
    (try (pure . Text <$> rightFlankingDelim delim prevJustClosed)) <|>
    -- Inline content
    (try (liftA2 (++)
      (inlineMarkdown isStartOfDelimited)
      (mdTillDelim False (delim, True)))) <|>
    -- Consume another token as text and recurse
    if prevJustClosed
      then -- Try the same, but allow the right flanking delim to consume an
           -- additional character
        mdTillDelim isStartOfDelimited (delim, False)
      else -- Consume another token and retry
        liftA2 (:) text $ mdTillDelim False (delim, False)

-- | Parses a block of inline content, delimited by * or **. If the length of
--   the delimiter is odd it first tries * and then **, and if even it tries
--   in opposite order. It is parameterized by whether it is the first
--   element in an enclosing context (so no previous tokens can exist).
--   Matches a list of Markdown so that if a text token had to be consumed to
--   create the delimiter it isn't lost.
emOrStrong :: Bool -> TokenParser [Markdown]
emOrStrong isStartOfDelimited = consumeInlineContent <$> do
  (_, maxdelim) <- lookAhead (try $ leftFlankingDelimAll isStartOfDelimited '*')
  let emphasis = emphasisP "*"
      strongemphasis = emphasisP "**"
  if even (length maxdelim)
    then try strongemphasis <|> emphasis
    else try emphasis <|> strongemphasis
  where
    consumeInlineContent :: (Maybe Char, Markdown) -> [Markdown]
    consumeInlineContent (Just c, m)  = [Text [c], m]
    consumeInlineContent (Nothing, m) = [m]

    emphasisP :: String -> TokenParser (Maybe Char, Markdown)
    emphasisP s = do
      (mC, delim) <- leftFlankingDelim isStartOfDelimited s
      -- To prevent the right flanking delimiter from matching immediately,
      -- we parameterize it with False. This forces the delimited section
      -- to include at least one token.
      content <- simplify <$> mdTillDelim True (delim, False)
      -- content <- mdTillDelim True $ Just (delim, False)
      guard (not $ null content)
      return (mC, (if length s == 1 then Italics else Bold) $ Many content)

-- | Unit test
trunInlineP :: Test
trunInlineP = TestList
  [ formTest "*hello world*" [Italics $ Many [Text "hello world"]]
  , formTest "**hello world**" [Bold $ Many [Text "hello world"]]
  , formTest "***hello world***" [Italics $ Many [Bold $ Many [Text "hello world"]]]
  , formTest "***hello* world**" [Bold $ Many [Italics $ Many [Text "hello"], Text " world"]]
  , formTest "***hello** world*" [Italics $ Many [Bold $ Many [Text "hello"], Text " world"]]
-- TODO disagreement. The js dingus says this next one should be
-- ***hello *<em>world</em>, but I think it should be <em>**hello **world</em>.
  , formTest "***hello **world*" [Italics $ Many [Text "**hello **world"]]
  , formTest "**hello** **world**" [Bold $ Many [Text "hello"], Text " ",Bold $ Many [Text "world"]]
  -- Inline Links
  , formTest "[hello](/world)" [Link "/world" Nothing $ Many [Text "hello"]]
  , formTest "[hello](/world (foo))" [Link "/world" (Just "foo") $ Many [Text "hello"]]
  ]
  where
  formTest s mdl = s ~: runInlineP s Map.empty ~?= mdl

-- | Parser that recognizes a right flanking delimiter run matching delim.
--   It returns the token that belongs to the content being delimited
--   that was consumed to find the delimiter (as the delimiter can't be
--   preceded by whitespace).
--   The delimiter is parameterized by a boolean, denoting whether the previous
--   delimiter just finished (so there won't be any tokens to consume)
-- TODO make it take (String,Bool) instead of taking them separately
rightFlankingDelim :: String -> Bool -> TokenParser String
rightFlankingDelim delim justFinishedPrev =
  if justFinishedPrev
    then
    -- If we just finished a delimited section, it must have had a punctuation
    -- character. Therefore we need only check that it is followed by whitespace
    -- or punctuation.
      (do
        _ <- punctParserSeq delim
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
          _ <- punctParserSeq delim
          return prev
        Just p  -> do
          -- preceded by punctuation p
          -- Must be followed by whitespace or punctuation
          _ <- punctParserSeq delim
          eof <|> void (lookAhead $ whitespaceParser <|> newLineParser <|> punctParser)
          return [p]

-- | Top level token parser for any kind of Markdown
inlineMarkdown :: Bool -> TokenParser [Markdown]
inlineMarkdown isStartOfDelimited = choice
  [ pure <$> inlineLink
  --, refLink
  , emOrStrong isStartOfDelimited
  , pure <$> text
  ]

-- Utilities for parsing individual types.
-- | Consumes one punctuation token. Fails if the next token isn't punctuation.
punctParser :: TokenParser Char
punctParser = tokenPrim show nextPos testMatch
  where
  nextPos   ps _ _ = incSourceColumn ps 1
  testMatch t      = case t of
                       Punctuation c -> Just c
                       _             -> Nothing

-- | Consumes one punctuation token, where the punctuation character is in s.
--   Fails if the next token isn't punctuation or isn't the right character.
punctParserS :: String -> TokenParser Char
punctParserS s = tokenPrim show nextPos testMatch
  where
  nextPos   ps _ _ = incSourceColumn ps 1
  testMatch t      = case t of
                       Punctuation c -> if c `elem` s then Just c else Nothing
                       _             -> Nothing

-- | Consumes a sequence of punctuation defined by the provided string. Fails
--   if it isn't matched (may consume input so should be used with try)
punctParserSeq :: String -> TokenParser String
punctParserSeq = foldr (\x -> liftA2 (:) (punctParserS [x])) (return [])

-- | Consumes one punctuation token unless the punctuation character is in s.
--   Fails if the next token isn't punctuation or is in the exception list.
punctParserN :: String -> TokenParser Char
punctParserN except = tokenPrim show nextPos testMatch
  where
  nextPos   ps _ _ = incSourceColumn ps 1
  testMatch t      = case t of
                       Punctuation c -> if c `elem` except then Nothing else Just c
                       _             -> Nothing

-- | Consumes a block of whitespace, yielding the whitespace character
--   encountered.
whitespaceParser :: TokenParser Char
whitespaceParser = tokenPrim show nextPos testMatch
  where
  nextPos   ps _ _ = incSourceColumn ps 1
  testMatch t      = case t of
                       Whitespace w -> Just w
                       _            -> Nothing

-- | Consumes a newline, yielding the \n character
newLineParser :: TokenParser Char
newLineParser = tokenPrim show nextPos testMatch
  where
  nextPos   ps _ _ = incSourceColumn ps 1
  testMatch t      = case t of
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
runEscapes (Punctuation '\\' : Punctuation x : xs)
  -- Escapable punctuation
  | x `elem` "*\\_[]<>()\"\'" = Word [x] : runEscapes xs
  -- Not escapable punctuation
  | otherwise                 = Word "\\" : runEscapes (Punctuation x : xs)
runEscapes (x:xs) = x : runEscapes xs
runEscapes []     = []

-- | Consumes an html pre, script, or style block and consumes all tokens until
--   it reaches its associated close tag. It then uses that to generate a Text
--   Markdown leaf.
preBlock :: TokenParser Markdown
preBlock = undefined

-- | Create a Markdown AST from the input string. Errors if the string can't be
--   fully consumed to create valid Markdown.

runInlineP :: String -> LinkRefMap -> [Markdown]
runInlineP s m = case parseOut of
                   Right result -> result
                   Left _       -> [Text s] -- TODO is this the right move?
  where parseOut = do tok <- tokenize s
                      runParser (concat <$> many (inlineMarkdown True)) m "" $ runEscapes tok

code :: TokenParser Markdown
code = undefined

italics :: TokenParser Markdown
italics = undefined

refLink :: TokenParser Markdown
refLink = undefined

inlineLink :: TokenParser Markdown
inlineLink = do
  -- There are many more rules for link text, but as a first pass anything goes
  linkText <- liftA Many $ (punctParserS "[") *>
                           manyTill (text) (try $ punctParserS "]")
  -- Not supporting surrounding destinations in <>.
  (dest, title) <- punctParserS "(" *> destTitle <* punctParserS ")"
  return $ Link dest title linkText
  where
  destTitle :: TokenParser (String, Maybe String)
  destTitle = (,) <$>
    (foldr (++) [] <$> manyTill
      anyTextString
      (lookAhead $ punctParserS ")" <|> whitespaceParser)) <*>
    option Nothing (try title)
  title :: TokenParser (Maybe String)
  title = (Just . foldr (++) []) <$>
      (whitespaceParser *> manyTill anyTextString (lookAhead $ punctParserS ")"))

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
  nextPos   ps _ _  = incSourceColumn ps 1
  testMatch t       = case t of
    Whitespace  w -> Just [w]
    Punctuation p -> Just [p]
    Word        w -> Just w
    NewLine       -> Just "\n"

-- | Consumes a word token and produces the contained value as a string.
textString :: TokenParser String
textString = tokenPrim show nextPos testMatch
  where
  nextPos   ps _ _  = incSourceColumn ps 1
  testMatch t       = case t of
    Word w -> Just w
    _      -> Nothing
