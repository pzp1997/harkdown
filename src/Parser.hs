module Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)

import AST
import ParserCombinators

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

blockP :: Parser [Partial]
blockP = manyTill (choice $ try <$> [ thematicBreak
                          -- , unorderedListItem
                          -- , orderedListItem
                          , blockquote
                          , atxHeading
                          , fencedCode
                          , blankLine *> pure PBlankLine
                          , paragraph
                          ]) eof

inlineP :: Parser Markdown
inlineP = undefined
-- inlineP = return . Text

----------------------------  BLOCK LEVEL PARSERS  ----------------------------

-- the Monoid instance for Map is a left biased union. CommonMark respects the
-- first occurence of a ref

type LinkRefMap = Map String (String, Maybe String)
type BlockLevel = Writer LinkRefMap Partial

emptyWriter :: Ord k => a -> Writer (Map k v) a
emptyWriter = writer . (\p -> (p, Map.empty))

thematicBreak, atxHeading,      setextHeading     :: Parser Partial
indentedCode,  fencedCode,      paragraph         :: Parser Partial
blockquote,    orderedListItem, unorderedListItem :: Parser Partial

thematicBreak = lineStart *> thematicMarker *> pure PHorizontalRule

atxHeading = do lineStart
                hLevel <- atxMarker
                content <- choice
                  [ some spaceChar *> manyTill (noneOf "\n\r")
                      (try $ spacesAround (many $ char '#') *> eolf)
                  , eolf
                  ]
                return $ PHeader hLevel content

setextHeading = undefined

indentedCode = undefined

fencedCode = do indentSize <- lineStart
                openFence <- fenceMarker
                infoString <- optionMaybe $ spacesAround (some nonWhiteSpace)
                eolf
                content <- manyTill (atMost indentSize spaceChar *> line) $
                  choice [ try (do lineStart
                                   closeFence <- fenceMarker
                                   eolf
                                   unless (openFence `isPrefixOf` closeFence) $
                                     fail "closing code fence")
                         , eof
                         ]
                return $ PCodeBlock infoString $ concat content

paragraph = PParagraph <$> (lineStart *> paragraphContent)

blockquote = PBlockQuote <$> (lineStart *> blockquoteMarker *> paragraphContent)

orderedListItem = do n <- lineStart
                     marker <- orderedListMarker
                     m <- repeatBetweenN 1 4 spaceChar
                     content <- listItemContent $ n + m + length marker
                     return $ POrderedList (read marker) content

unorderedListItem = do n <- lineStart
                       m <- unorderedListMarker
                       content <- listItemContent $ n + m
                       return $ PUnorderedList content

linkRef :: Parser BlockLevel
linkRef = undefined

----------------------------  INLINE LEVEL PARSERS  ---------------------------

code, italics, bold, link, image :: Parser Markdown

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
                                                  , () <$ orderedListMarker
                                                  , () <$ unorderedListMarker
                                                  , blockquoteMarker
                                                  ]

thematicMarker :: Parser ()
thematicMarker = choice (atLeast_ 3 . breakChar <$> "*-_") <* eolf <?> "thematic break"
  where breakChar c = char c <* many spaceChar

atxMarker :: Parser Int
atxMarker = repeatBetweenN 1 6 (char '#') <?> "ATX heading"

fenceMarker :: Parser String
fenceMarker = choice (atLeast 3 . char <$> "`~") <?> "fenced code block"

orderedListMarker :: Parser String
orderedListMarker = repeatBetween 1 9 digit <* choice (char <$> ".)") <?> "ordered list"

unorderedListMarker :: Parser Int
unorderedListMarker = (choice (char <$> "-+*") *> repeatBetweenN 1 4 spaceChar) <?> "unordered list"

blockquoteMarker :: Parser ()
blockquoteMarker = (char '>' *> optional (char ' ') *> return ()) <?> "blockquote"

setextMarker :: Parser Int
setextMarker = (1 <$ some (char '=') <|> 2 <$ some (char '-')) <* eolf

-------------------------------  HELPER PARSERS  ------------------------------

backtickString :: Parser String
backtickString = some $ char '`'

paragraphContent :: Parser String
-- paragraphContent = manyTill anyChar $ try (eol *> lookAhead blankLine_) <|> eof
paragraphContent = manyTill anyChar $
  try (eol *> ((lineStart *> interruptMarkers) <|> blankLine)) <|> eof

-- containerParser :: Parser a -> Parser String
-- containerParser marker = manyTill anyChar $
--   try (eol *> blankLine_ <|> (() <$ lookAhead marker)) <|> eof

-- TODO this should actually stop parsing whenever ANY block level marker
-- that is indented less than w spaces is found. Can be re-used for blockquote
-- with the special case of w = 4.
continueContent :: Parser String
continueContent = manyTill anyChar $ try $ choice
                    [ eol *> choice [ lineStart *> interruptMarkers
                                    , blankLine
                                    ]
                    , eof
                    ]


listItemContent = undefined

-- listItemContent :: Int -> Parser String
-- listItemContent = container $ blankLine *> blankLine -- TODO this is wrong

-- blockquoteContent :: Parser String
-- blockquoteContent = manyTill anyChar $
--   try (eol *> lookAhead (lineStart <* interruptMarkers) <|> blankLine)
--   <|> eof

-- blockquoteContinutation :: Parser String
-- blockquoteContinutation = containerParser blockquoteMarker
--
-- orderedListContinutation :: Parser String
-- orderedListContinutation = containerParser orderedListMarker

----------------------------  DEFINITIONAL PARSERS  ---------------------------

eol :: Parser String
eol =   string "\n"
    <|> liftA2 (++) (string "\r") (string "\n" <|> string "")
    <?> "end of line"

eolf :: Parser String
eolf = eol <|> "" <$ eof

line :: Parser String
line = manyTillEnd anyChar eolf

blankLine :: Parser ()
blankLine = do rawLine <- line
               if all isSpace rawLine then return ()
               else fail "blank line"

lineStart :: Parser Int
lineStart = atMostN 3 spaceChar

-- TODO deal with escaped brackets here
linkLabel :: Parser String
linkLabel = between (char '[') (char ']') contentP
  where contentP = do ref <- repeatBetween 1 999 (noneOf "[]")
                      if all isSpace ref then fail "valid link ref"
                      else return $ condenseSpace ref


-------------------------------  STRING HELPERS  ------------------------------

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
        combine x xs      = (x:) <$> xs
