module Parser where

import Control.Applicative
import Control.Monad
-- import Control.Monad.Writer -- TODO do we need this
import Data.Char (isSpace)
import Data.List (isPrefixOf)
-- import Data.Map (Map)       -- TODO do we need this
-- import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)

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

mainP :: String -> [Markdown]
mainP s = case parse (connector <$> blockP) "" (newlineTerminate s) of
            Left  _ -> []
            Right x -> x

connector :: [Partial] -> [Markdown]

connector (PUnorderedListItem c x : rest) =
  connector $ PUnorderedList c True [Many $ mainP x] : rest
connector (PUnorderedList c tight xs : PUnorderedListItem c' x : rest)
  | c == c' = connector $ PUnorderedList c tight (xs ++ [Many $ mainP x]) : rest
connector (PUnorderedList c _ xs : PBlankLine : rest@(PUnorderedListItem c' _ : _))
  | c == c' = connector $ PUnorderedList c False xs : rest
connector (ul@PUnorderedList{} : PBlankLine : rest) = connector $ ul : rest
connector (PUnorderedList _ tight xs : rest) =
  UnorderedList tight xs : connector rest

connector (POrderedListItem n c x : rest) =
  connector $ POrderedList n c True [Many $ mainP x] : rest
connector (POrderedList n c tight xs : POrderedListItem _ c' x : rest)
  | c == c' = connector $ POrderedList n c tight (xs ++ [Many $ mainP x]) : rest
connector (POrderedList n c _ xs : PBlankLine : rest@(POrderedListItem _ c' _ : _))
  | c == c' = connector $ POrderedList n c False xs : rest
connector (ol@POrderedList{} : PBlankLine : rest) = connector $ ol : rest
connector (POrderedList n _ tight xs : rest) =
  OrderedList n tight xs : connector rest

connector (PBlockQuote s : PBlockQuote t : rest) = connector $ PBlockQuote (s ++ t) : rest
connector (PBlockQuote s : rest) = BlockQuote (Many $ mainP s) : connector rest

connector (PHeader level s : rest) = Header level (Many $ runInlineP s) : connector rest
connector (PHorizontalRule : rest) = HorizontalRule : connector rest
connector (PCodeBlock maybeInfo s : rest) = CodeBlock maybeInfo s : connector rest
connector (PParagraph s : rest) = Paragraph (Many $ runInlineP $ trim s) : connector rest
connector (PBlankLine : rest) = connector rest
connector [] = []

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

-- inlineP :: Parser Markdown
-- inlineP = Text <$> many anyChar

-- runInlineP :: String -> Markdown
-- runInlineP s = case parse inlineP "" s of
--                  Left  _ -> undefined -- TODO replace with something sensible
--                  Right x -> x


-- inlineP = return . Text

----------------------------  BLOCK LEVEL PARSERS  ----------------------------

-- the Monoid instance for Map is a left biased union. CommonMark respects the
-- first occurence of a ref

-- type LinkRefMap = Map String (String, Maybe String)
-- type BlockLevel = Writer LinkRefMap Partial
--
-- emptyWriter :: Ord k => a -> Writer (Map k v) a
-- emptyWriter = writer . (\p -> (p, Map.empty))

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

fencedCode = do indentSize <- lineStart
                openFence <- fenceMarker
                infoString <- line
                content <- manyTill (atMost indentSize spaceChar *> line) $
                             try (close openFence) <|> eof
                return $ PCodeBlock (trim infoString) $ concat content
  where close f = do _ <- lineStart
                     closeFence <- fenceMarker
                     _ <- eol
                     unless (f `isPrefixOf` closeFence) $
                       fail "closing code fence"

paragraph = PParagraph <$> (lineStart *> continutation 3)

blockquote = PBlockQuote <$> (lineStart *> blockquoteMarker *> continutation 3)

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

linkRef :: Parser String
linkRef = undefined
-- linkRef = lineStart *> linkLabel <*> char ':' <* spacesAround (optional eol) <* linkDestination <*> spacesAround (optional eol)
--   where linkDestination = between (char '<') (char '>') (many $ noneOf " \t\v\n\r<>") <|> fail "TODO" -- TODO figure out what they mean by matching parens
--         linkTitle = undefined

----------------------------  INLINE LEVEL PARSERS  ---------------------------

code, italics, bold, link, image, autolink, text :: Parser Markdown

code = undefined
italics = undefined
bold = undefined
link = undefined
image = undefined
autolink = undefined
text = undefined

----------------------------------- MARKERS -----------------------------------

interruptMarkers :: Parser ()
interruptMarkers = choice $ (try . lookAhead) <$> [ thematicMarker
                                                  , () <$ atxMarker
                                                  , () <$ fenceMarker
                                                  , orderedListMarker *> repeatBetween 1 4 spaceChar *> nonWhiteSpaceChar *> return ()
                                                  , unorderedListMarker *> repeatBetween 1 4 spaceChar *> nonWhiteSpaceChar *> return ()
                                                  , blockquoteMarker
                                                  , () <$ blankLine
                                                  ]

thematicMarker :: Parser ()
thematicMarker = choice (atLeast_ 3 . breakChar <$> "*-_") <* eol <?> "thematic break"
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

continutation :: Int -> Parser String
-- continutation = manyTill anyChar $ try (eol *> lookAhead blankLine_) <|> eof
continutation w = manyTillEnd anyChar $ try stop <|> "" <$ eof
  where stop = eol <* atMost w spaceChar <* interruptMarkers

-- TODO not quite right. after the first chunk all others must be indented by at least w
-- listItemContent :: Int -> Parser String
-- listItemContent w = concat <$> sepBy1 (continutation w) (some $ try blankLine)

listItemContent :: Int -> Parser String
listItemContent w = do first <- continutation w
                       rest <- many $ try $ liftA2 (\xs x -> concat $ xs ++ [x])
                                 (some $ try blankLine)
                                 (atLeast (w + 1) spaceChar *> continutation w)
                       return . concat $ first : rest
  --
  --  concat <$> liftA2 (:) (continutation w) (option $ blankLines *>
  -- sepBy (atLeast (w + 1) spaceChar *> continutation w) blankLines)


----------------------------  DEFINITIONAL PARSERS  ---------------------------

line :: Parser String
line = manyTillEnd anyChar eol

blankLine :: Parser String
blankLine = manyTillEnd (oneOf " \t\v") eol

nonWhiteSpaceChar :: Parser Char
nonWhiteSpaceChar = satisfy (not . isSpace)

lineStart :: Parser Int
lineStart = atMostN 3 spaceChar

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
