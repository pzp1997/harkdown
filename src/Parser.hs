module Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Text.Parsec hiding (many, optional, (<|>), State)
import Text.Parsec.String (Parser)

import AST
import ParserCombinators
import InlineParsers (runInlineP)

--------------------------------  BIG PARSERS  --------------------------------

type LinkRefMap = Map String String

runMainP :: String -> [Markdown]
runMainP s = uncurry connector $ runState (runBlockP $ newlineTerminate s) Map.empty

runBlockP :: String -> State LinkRefMap [Partial]
runBlockP s = case parse blockP "" s of
                Left  _ -> return []
                Right x -> combiner x

combiner :: [Partial] -> State LinkRefMap [Partial]

combiner (PUnorderedListItem c x : rest) = do
  x' <- runBlockP x
  combiner $ PUnorderedList c True [x'] : rest
combiner (PUnorderedList c tight xs : PUnorderedListItem c' x : rest)
  | c == c' = do
      x' <- runBlockP x
      combiner $ PUnorderedList c tight (xs ++ [x']) : rest
combiner (PUnorderedList c _ xs : PBlankLine : rest@(PUnorderedListItem c' _ : _))
  | c == c' = combiner $ PUnorderedList c False xs : rest
combiner (ul@PUnorderedList{} : PBlankLine : rest) = combiner $ ul : rest

combiner (POrderedListItem n c x : rest) = do
  x' <- runBlockP x
  combiner $ POrderedList n c True [x'] : rest
combiner (POrderedList n c tight xs : POrderedListItem _ c' x : rest)
  | c == c' = do x' <- runBlockP x
                 combiner $ POrderedList n c tight (xs ++ [x']) : rest
combiner (POrderedList n c _ xs : PBlankLine : rest@(POrderedListItem _ c' _ : _))
  | c == c' = combiner $ POrderedList n c False xs : rest
combiner (ol@POrderedList{} : PBlankLine : rest) = combiner $ ol : rest

combiner (PBlockQuoteItem x : PBlockQuoteItem y : rest) = combiner $ PBlockQuoteItem (x ++ y) : rest
combiner (PBlockQuoteItem x : rest) = do x' <- runBlockP x
                                         rest' <- combiner rest
                                         return $ PBlockQuote x' : rest'

combiner (PLinkRef ref dest : rest) = do
  modify $ Map.insertWith (flip const) ref dest
  combiner rest

combiner (x : xs) = do xs' <- combiner xs
                       return $ x : xs'
combiner [] = return []

connector :: [Partial] -> LinkRefMap -> [Markdown]
connector (PUnorderedList _ tight xs : rest) m =
  UnorderedList tight ((Many . (`connector` m)) <$> xs) : connector rest m
connector (POrderedList n _ tight xs : rest) m =
  OrderedList n tight ((Many . (`connector` m)) <$> xs) : connector rest m
connector (PBlockQuote xs : rest) m =
  BlockQuote (Many $ connector xs m) : connector rest m
connector (PHeader level s : rest) m =
  Header level (Many $ runInlineP s m) : connector rest m
connector (PHorizontalRule : rest) m =
  HorizontalRule : connector rest m
connector (PCodeBlock maybeInfo s : rest) m =
  CodeBlock maybeInfo s : connector rest m
connector (PParagraph s : rest) m =
  Paragraph (Many $ runInlineP (trim s) m) : connector rest m
connector (_ : rest) m = connector rest m
connector [] _ = []

blockP :: Parser [Partial]
blockP = manyTill (choice $ try <$> [ thematicBreak
                                    , unorderedListItem
                                    , orderedListItem
                                    , blockquote
                                    , atxHeading
                                    , fencedCode
                                    , blankLine *> pure PBlankLine
                                    , paragraph
                                    ]) eof

----------------------------  BLOCK LEVEL PARSERS  ----------------------------

thematicBreak, atxHeading,      setextHeading     :: Parser Partial
indentedCode,  fencedCode,      paragraph         :: Parser Partial
blockquote,    orderedListItem, unorderedListItem :: Parser Partial

thematicBreak = lineStart *> thematicMarker *> pure PHorizontalRule

atxHeading = do _ <- lineStart
                hLevel <- atxMarker
                content <- choice
                  [ some spaceChar *> manyTill (noneOf "\n\r")
                      (try $ spacesAround (many $ char '#') *> eol)
                  , eol
                  ]
                return $ PHeader hLevel content

setextHeading = undefined

indentedCode = (PCodeBlock "" . concat) <$> some (try $ indentedLine <|> blankLine)
  where indentedLine = atLeast 4 spaceChar *> line -- TODO might be able to use line here

fencedCode = do
  w <- lineStart
  openFence <- fenceMarker
  infoString <- line
  content <- manyTill (try blankLine <|> consumeUpto w spaceChar *> line) $
               try (close openFence) <|> eof
  return $ PCodeBlock (trim infoString) $ concat content
  where close f = do
          _ <- lineStart
          closeFence <- fenceMarker
          _ <- eol
          unless (f `isPrefixOf` closeFence) $
            fail "closing code fence"

paragraph = PParagraph <$> (lineStart *> continuation 3)

blockquote = PBlockQuoteItem <$> (lineStart *> blockquoteMarker *> continuation 3)

orderedListItem = do n <- lineStart
                     (index, delim) <- orderedListMarker
                     m <- repeatBetweenN 1 4 spaceChar
                     content <- listItemContent $ n + m + length index
                     return $ POrderedListItem (read index) delim content

unorderedListItem = do n <- lineStart
                       delim <- unorderedListMarker
                       m <- repeatBetweenN 1 4 spaceChar
                       content <- listItemContent $ n + m
                       return $ PUnorderedListItem delim content

linkRef :: Parser Partial
linkRef = do
  _ <- lineStart
  l <- linkLabel
  _ <- char ':'
  _ <- spacesAround (optional eol)
  d <- linkDestination
  _ <- blankLine -- <* spacesAround (optional eol)
  return $ PLinkRef l d

linkDestination = between (char '<') (char '>') (many $ noneOf " \t\v\n\r<>") <|> fail "TODO" -- TODO figure out what they mean by matching parens
linkTitle = undefined

----------------------------------- MARKERS -----------------------------------

interruptMarkers :: Parser ()
interruptMarkers = choice $ (try . lookAhead) <$>
  [ void thematicMarker
  , void atxMarker
  , void fenceMarker
  , orderedListMarker *> repeatBetween 1 4 spaceChar *> nonWhiteSpaceChar *> return ()
  , unorderedListMarker *> repeatBetween 1 4 spaceChar *> nonWhiteSpaceChar *> return ()
  , blockquoteMarker
  , void blankLine
  ]

thematicMarker :: Parser String
thematicMarker = choice (atLeast 3 . breakChar <$> "*-_") <* eol <?> "thematic break"
  where breakChar c = char c <* many spaceChar

atxMarker :: Parser Int
atxMarker = repeatBetweenN 1 6 (char '#') <?> "ATX heading"

fenceMarker :: Parser String
fenceMarker = choice (atLeast 3 . char <$> "`~") <?> "fenced code block"

orderedListMarker :: Parser (String, Char)
orderedListMarker = liftA2 (,) (repeatBetween 1 9 digit) (choice $ char <$> ".)") <?> "ordered list"

unorderedListMarker :: Parser Char
unorderedListMarker = choice (char <$> "-+*") <?> "unordered list"

blockquoteMarker :: Parser ()
blockquoteMarker = (char '>' *> optional (char ' ') *> return ()) <?> "blockquote"

setextMarker :: Parser Int
setextMarker = (1 <$ some (char '=') <|> 2 <$ some (char '-')) <* eol

-------------------------------  HELPER PARSERS  ------------------------------

backtickString :: Parser String
backtickString = some $ char '`'

continuation :: Int -> Parser String
continuation w = manyTillEnd anyChar $ try stop <|> "" <$ eof
  where stop = eol <* atMost w spaceChar <* interruptMarkers

listItemContent :: Int -> Parser String
listItemContent w = do first <- continuation w
                       rest <- many $ try $ liftA2 (\xs x -> concat $ xs ++ [x])
                                 (some $ try blankLine)
                                 (atLeast (w + 1) spaceChar *> continuation w)
                       return . concat $ first : rest


----------------------------  DEFINITIONAL PARSERS  ---------------------------

line :: Parser String
line = manyTillEnd anyChar eol

blankLine :: Parser String
blankLine = manyTillEnd (oneOf " \t\v") eol

nonWhiteSpaceChar :: Parser Char
nonWhiteSpaceChar = satisfy (not . isSpace)

lineStart :: Parser Int
lineStart = length <$> atMost 3 spaceChar

-- TODO deal with escaped brackets here
linkLabel :: Parser String
linkLabel = between (char '[') (char ']') contentP
  where contentP = do ref <- repeatBetween 1 999 (noneOf "[]")
                      if all isSpace ref
                        then fail "valid link ref"
                        else return (condenseSpace ref)


-------------------------------  STRING HELPERS  ------------------------------

newlineTerminate :: String -> String
newlineTerminate ""   = "\n"
newlineTerminate "\n" = "\n"
newlineTerminate (c : cs) = c : newlineTerminate cs

condenseSpace :: String -> String
condenseSpace = helper . dropWhile isSpace
  where helper [x]
          | isSpace x              = ""
        helper [x, y]
          | isSpace x && isSpace y = ""
        helper (x : xs@(y : ys))
          | isSpace x && isSpace y = helper $ ' ' : ys
          | otherwise              = x : helper xs
        helper a                   = a

trim :: String -> String
trim = dropWhileRight isSpace . dropWhile isSpace

dropWhileRight :: (a -> Bool) -> [a] -> [a]
dropWhileRight p = fromMaybe [] . foldr combine Nothing
  where combine x Nothing = if p x then Nothing else Just [x]
        combine x xs      = (x : ) <$> xs
