module Parser where

import Control.Applicative
import Control.Monad
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Text.Parsec hiding (many, optional, (<|>))
-- import Text.Parsec.String (Parser)

import AST
import ParserCombinators
import InlineParsers (runInlineP)

-- parseMarkdown input = case parse (many atxHeading) "" input of
--                         Left  _  -> []
--                         Right md -> md

--------------------------------  BIG PARSERS  --------------------------------

-- markdownP :: Parser [Markdown]
-- markdownP = do writer <- blockP
--                let (partialAst, linkRefs) = runWriter writer
--                return []

-- once we have list of partials, need to go through and group runs of
-- blockquotes, list items.

type UserState = Map String String
type BlockParser = Parsec String UserState

runMainP :: String -> [Markdown]
runMainP s = case runParser (connector <$> blockP) Map.empty "" (newlineTerminate s) of
               Left  _ -> []
               Right x -> x

runBlockP :: String -> UserState -> ([Partial], UserState)
runBlockP s u = case runParser wrapBlockP u "" s of
                  Left  _ -> ([], Map.empty)
                  Right x -> x
  where wrapBlockP = do blocks <- blockP
                        st <- getState
                        return (blocks, st)

combiner :: [Partial] -> ([Partial], Map String String)
combiner = undefined


connector :: [Partial] -> [Markdown]

connector (PUnorderedListItem c x : rest) =
  connector $ PUnorderedList c True [Many $ runMainP x] : rest
connector (PUnorderedList c tight xs : PUnorderedListItem c' x : rest)
  | c == c' = connector $ PUnorderedList c tight (xs ++ [Many $ runMainP x]) : rest
connector (PUnorderedList c _ xs : PBlankLine : rest@(PUnorderedListItem c' _ : _))
  | c == c' = connector $ PUnorderedList c False xs : rest
connector (ul@PUnorderedList{} : PBlankLine : rest) = connector $ ul : rest
connector (PUnorderedList _ tight xs : rest) =
  UnorderedList tight xs : connector rest

connector (POrderedListItem n c x : rest) =
  connector $ POrderedList n c True [Many $ runMainP x] : rest
connector (POrderedList n c tight xs : POrderedListItem _ c' x : rest)
  | c == c' = connector $ POrderedList n c tight (xs ++ [Many $ runMainP x]) : rest
connector (POrderedList n c _ xs : PBlankLine : rest@(POrderedListItem _ c' _ : _))
  | c == c' = connector $ POrderedList n c False xs : rest
connector (ol@POrderedList{} : PBlankLine : rest) = connector $ ol : rest
connector (POrderedList n _ tight xs : rest) =
  OrderedList n tight xs : connector rest

connector (PBlockQuote xs : PBlockQuote ys : rest) = connector $ PBlockQuote (xs ++ ys) : rest
connector (PBlockQuote xs : rest) = BlockQuote (Many $ connector xs) : connector rest

connector (PHeader level s : rest) = Header level (Many $ runInlineP s) : connector rest
connector (PHorizontalRule : rest) = HorizontalRule : connector rest
connector (PCodeBlock maybeInfo s : rest) = CodeBlock maybeInfo s : connector rest
connector (PParagraph s : rest) = Paragraph (Many $ runInlineP $ trim s) : connector rest
connector (PBlankLine : rest) = connector rest
connector (PLinkRef : rest) = connector rest
connector [] = []

blockP :: BlockParser [Partial]
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

-- the Monoid instance for Map is a left biased union. CommonMark respects the
-- first occurence of a ref

-- type LinkRefMap = Map String (String, Maybe String)
-- type BlockLevel = Writer LinkRefMap Partial
--
-- emptyWriter :: Ord k => a -> Writer (Map k v) a
-- emptyWriter = writer . (\p -> (p, Map.empty))

thematicBreak, atxHeading,      setextHeading     :: BlockParser Partial
indentedCode,  fencedCode,      paragraph         :: BlockParser Partial
blockquote,    orderedListItem, unorderedListItem :: BlockParser Partial

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

blockquote = do
  _ <- lineStart
  blockquoteMarker
  content <- continuation 3
  st <- getState
  let (partials, st') = runBlockP content st
  putState st'
  return $ PBlockQuote partials

  -- (PBlockQuote . runBlockP) <$> (lineStart *> blockquoteMarker *> continuation 3)

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

linkRef :: BlockParser Partial
linkRef = do
  _ <- lineStart
  l <- linkLabel
  _ <- char ':'
  _ <- spacesAround (optional eol)
  d <- linkDestination
  _ <- blankLine -- <* spacesAround (optional eol)
  modifyState $ Map.insertWith (flip const) l d
  return PLinkRef

linkDestination = between (char '<') (char '>') (many $ noneOf " \t\v\n\r<>") <|> fail "TODO" -- TODO figure out what they mean by matching parens
linkTitle = undefined

----------------------------  INLINE LEVEL PARSERS  ---------------------------

-- code, italics, bold, link, image, autolink, text :: Parser Markdown
--
-- code = undefined
-- italics = undefined
-- bold = undefined
-- link = undefined
-- image = undefined
-- autolink = undefined
-- text = undefined

----------------------------------- MARKERS -----------------------------------

interruptMarkers :: BlockParser ()
interruptMarkers = choice $ (try . lookAhead) <$>
  [ void thematicMarker
  , void atxMarker
  , void fenceMarker
  , orderedListMarker *> repeatBetween 1 4 spaceChar *> nonWhiteSpaceChar *> return ()
  , unorderedListMarker *> repeatBetween 1 4 spaceChar *> nonWhiteSpaceChar *> return ()
  , blockquoteMarker
  , void blankLine
  ]

thematicMarker :: BlockParser String
thematicMarker = choice (atLeast 3 . breakChar <$> "*-_") <* eol <?> "thematic break"
  where breakChar c = char c <* many spaceChar

atxMarker :: BlockParser Int
atxMarker = repeatBetweenN 1 6 (char '#') <?> "ATX heading"

fenceMarker :: BlockParser String
fenceMarker = choice (atLeast 3 . char <$> "`~") <?> "fenced code block"

orderedListMarker :: BlockParser (String, Char)
orderedListMarker = liftA2 (,) (repeatBetween 1 9 digit) (choice $ char <$> ".)") <?> "ordered list"

unorderedListMarker :: BlockParser Char
unorderedListMarker = choice (char <$> "-+*") <?> "unordered list"

blockquoteMarker :: BlockParser ()
blockquoteMarker = (char '>' *> optional (char ' ') *> return ()) <?> "blockquote"

setextMarker :: BlockParser Int
setextMarker = (1 <$ some (char '=') <|> 2 <$ some (char '-')) <* eol

-------------------------------  HELPER PARSERS  ------------------------------

backtickString :: BlockParser String
backtickString = some $ char '`'

continuation :: Int -> BlockParser String
continuation w = manyTillEnd anyChar $ try stop <|> "" <$ eof
  where stop = eol <* atMost w spaceChar <* interruptMarkers

listItemContent :: Int -> BlockParser String
listItemContent w = do first <- continuation w
                       rest <- many $ try $ liftA2 (\xs x -> concat $ xs ++ [x])
                                 (some $ try blankLine)
                                 (atLeast (w + 1) spaceChar *> continuation w)
                       return . concat $ first : rest


----------------------------  DEFINITIONAL PARSERS  ---------------------------

line :: BlockParser String
line = manyTillEnd anyChar eol

blankLine :: BlockParser String
blankLine = manyTillEnd (oneOf " \t\v") eol

nonWhiteSpaceChar :: BlockParser Char
nonWhiteSpaceChar = satisfy (not . isSpace)

lineStart :: BlockParser Int
lineStart = length <$> atMost 3 spaceChar

-- TODO deal with escaped brackets here
linkLabel :: BlockParser String
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
