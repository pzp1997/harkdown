module BlockParser where

import Control.Applicative
import Control.Monad
import Control.Monad.State (State, modify)
import Data.Char (isControl, isSpace)
import Data.Either (either)
import Data.List (isPrefixOf)
import qualified Data.Map as M (insertWith)

import Text.Parsec hiding (State, many, optional, (<|>))
import Text.Parsec.String (Parser)

import AST
import ParserCombinators

runBlockP :: String -> State LinkRefMap [Partial]
runBlockP s = either (const $ return []) combiner $ parse blockP "" s

blockP :: Parser [Partial]
blockP = manyTill (choice $ try <$> [ thematicBreak
                                    , unorderedListItem
                                    , orderedListItem
                                    , blockquote
                                    , atxHeading
                                    , fencedCode
                                    , blankLine *> pure PBlankLine
                                    , linkRef
                                    , paragraph
                                    ]) eof


----------------------------  BLOCK LEVEL PARSERS  ----------------------------

thematicBreak, atxHeading, setextHeading, indentedCode, fencedCode, paragraph,
  blockquote, orderedListItem, unorderedListItem, linkRef :: Parser Partial

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
  where indentedLine = atLeast 4 spaceChar *> line

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

linkRef = liftA3 PLinkRef
  (lineStart *> linkLabel)
  (char ':' *> spacesAround (optional eol) *> linkDestination)
  (Just <$> try (some spaceChar *> optional eol *> many spaceChar *> linkTitle <* blankLine)
     <|> (Nothing <$ spacesAround (optional eol)))

linkDestination :: Parser String
linkDestination = between (char '<') (char '>') (many $ noneOf " \t\v\n\r<>") <|> many (satisfy $ \c -> not (isControl c || isSpace c))

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

line :: Parser String
line = manyTillEnd anyChar eol

blankLine :: Parser String
blankLine = manyTillEnd (oneOf " \t\v") eol

lineStart :: Parser Int
lineStart = length <$> atMost 3 spaceChar


continuation :: Int -> Parser String
continuation w = manyTillEnd anyChar $ try stop <|> "" <$ eof
  where stop = eol <* atMost w spaceChar <* interruptMarkers

listItemContent :: Int -> Parser String
listItemContent w = do first <- continuation w
                       rest <- many $ try $ liftA2 (\xs x -> concat $ xs ++ [x])
                                 (some $ try blankLine)
                                 (atLeast (w + 1) spaceChar *> continuation w)
                       return . concat $ first : rest




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

combiner (PLinkRef ref dest title : rest) = do
  modify $ M.insertWith (flip const) ref (dest, title)
  combiner rest

combiner (x : xs) = do xs' <- combiner xs
                       return $ x : xs'
combiner [] = return []
