module Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (words, lines)

import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)

import AST
import ParserCombinators

-- parseMarkdown input = case parse (many atxHeading) "" input of
--                         Left  _  -> []
--                         Right md -> md

--------------------------------  BIG PARSERS  --------------------------------

markdownP :: Parser [Markdown]
markdownP = do writer <- blockP
               let (partialAst, linkRefs) = runWriter writer
               return []

blockP :: Parser BlockLevel
blockP = undefined

inlineP :: Parser Markdown
inlineP = undefined
-- inlineP = return . Text

----------------------------  BLOCK LEVEL PARSERS  ----------------------------

-- the Monoid instance for Map is a left biased union

type LinkRefMap = Map String (String, Maybe String)
type BlockLevel = Writer LinkRefMap Partial

emptyWriter :: Ord k => a -> Writer (Map k v) a
emptyWriter = writer . (\p -> (p, Map.empty))

thematicBreak, atxHeading,      setextHeading     :: Parser BlockLevel
indentedCode,  fencedCode,      paragraph         :: Parser BlockLevel
blockquote,    orderedListItem, unorderedListItem :: Parser BlockLevel

thematicBreak =  lineStart
              *> thematicMarker
              *> eol
              *> pure (return PHorizontalRule)

atxHeading = do lineStart
                hLevel <- atxMarker
                content <- some spaceChar *> manyTill (noneOf "\n\r")
                  (try $ spacesAround (many $ char '#') *> eol) <|> "" <$ eol
                return $ return $ PHeader hLevel content

setextHeading = undefined

indentedCode = undefined

fencedCode = do indent <- atMost 3 spaceChar
                let indentSize = length indent
                openFence <- fence
                infoString <- optionMaybe $ spacesAround $ some nonWhiteSpace
                content <- manyTill (atMost indentSize spaceChar *> litLine) $
                  eof <|> try (do closeFence <- fence
                                  unless (openFence `isPrefixOf` closeFence) $
                                    parserFail "closing code fence")
                return $ return $ PCodeBlock infoString $ concat content
  where fence = choice $ atLeast 3 . char <$> "`~"
        litLine = liftA2 (++) words eol

paragraph = (emptyWriter . PParagraph) <$> continuationText

blockquote = (emptyWriter . PBlockQuote) <$>
  (lineStart *> blockquoteMarker *> blockquoteContent)

orderedListItem = do n <- atMostN 3 spaceChar
                     marker <- orderedListMarker
                     m <- repeatBetweenN 1 4 spaceChar
                     content <- listItemContent $ n + m + length marker
                     return $ return $ POrderedList (read marker) content

unorderedListItem = do n <- atMostN 3 spaceChar
                       unorderedListMarker
                       m <- repeatBetweenN 1 4 spaceChar
                       content <- listItemContent $ n + m
                       return $ return $ PUnorderedList content

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
interruptMarkers = choice [ thematicMarker
                          , () <$ atxMarker
                          , () <$ orderedListMarker
                          , () <$ unorderedListMarker
                          , blockquoteMarker
                          ]

thematicMarker :: Parser ()
thematicMarker = choice $ atLeast_ 3 . breakChar <$> "*-_"
  where breakChar c = char c <* many spaceChar

atxMarker :: Parser Int
atxMarker = repeatBetweenN 1 6 $ char '#'

orderedListMarker :: Parser String
orderedListMarker = repeatBetween 1 9 digit <* choice (char <$> ".)")

unorderedListMarker :: Parser Char
unorderedListMarker = choice $ char <$> "-+*"

blockquoteMarker :: Parser ()
blockquoteMarker = char '>' *> optional (char ' ') *> return ()

setextMarker :: Parser Int
setextMarker = (1 <$ some (char '=') <|> 2 <$ some (char '-')) <* eolf


-------------------------------  HELPER PARSERS  ------------------------------

backtickString :: Parser String
backtickString = some $ char '`'

continuationText :: Parser String
continuationText = manyTill anyChar $ try (eol *> blankLine_) <|> eof

-- containerParser :: Parser a -> Parser String
-- containerParser marker = manyTill anyChar $
--   try (eol *> blankLine_ <|> (() <$ lookAhead marker)) <|> eof

-- TODO this should actually stop parsing whenever ANY block level marker
-- that is indented less than w spaces is found. Can be re-used for blockquote
-- with the special case of w = 4.
container :: Parser () -> Int -> Parser String
container blank w = manyTill anyChar $
  try (eol *> lookAhead
                ((atMost_ w spaceChar <* interruptMarkers)
                 <|> blank))
  <|> eof

listItemContent :: Int -> Parser String
listItemContent = container $ blankLine_ *> blankLine_

blockquoteContent :: Parser String
blockquoteContent = container blankLine_ 3

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

eof' :: Parser String
eof' = "" <$ eof

eolf :: Parser String
eolf = eol <|> eof'

words :: Parser String
words = many $ noneOf "\n\r"

line :: Parser String
line = words <* eolf

lines :: Parser [String]
lines = endBy words eolf

blankLine :: Parser Partial
blankLine = many (oneOf " \t") *> eolf *> pure PBlankLine

blankLine_ :: Parser ()
blankLine_ = () <$ blankLine_

lineStart :: Parser ()
lineStart = atMost_ 3 spaceChar

-- TODO deal with escaped brackets here
linkLabel :: Parser String
linkLabel = between (char '[') (char ']') contentP
  where contentP = do ref <- repeatBetween 1 999 (noneOf "[]")
                      if all isSpace ref then fail "valid link ref"
                      else return $ condenseSpace ref

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

-- condenseSpace' :: String -> String
-- condenseSpace' = unwords . words
