module Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
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

type BlockLevel = Writer (Map String String) Partial

thematicBreak, atxHeading, setextHeading :: Parser BlockLevel
indentedCode, fencedCode, paragraph      :: Parser BlockLevel
blockquote, orderedList, unorderedList   :: Parser BlockLevel

thematicBreak =  atMost_ 3 spaceChar
              *> choice (atLeast_ 3 . breakChar <$> "*-_")
              *> eol
              *> pure (return PHorizontalRule)
  where breakChar c = char c <* many spaceChar

atxHeading = do atMost_ 3 spaceChar
                hLevel <- repeatBetweenN 1 6 hashtagChar
                content <- some spaceChar *> manyTill (noneOf "\n\r")
                  (try $ spacesAround (many hashtagChar) *> eol) <|> "" <$ eol
                return $ return $ PHeader hLevel content
  where hashtagChar = char '#'

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

paragraph = (writer . (\p -> (p, Map.empty)) . PParagraph) <$>
              manyTill anyChar (try blankLine <|> eof')

blockquote = pure $ PBlockQuote <$>
  some (atMost_ 3 spaceChar *> char '>' *> optional (char ' ') *> blockP)

orderedList = undefined

unorderedList = undefined

linkRef :: Parser (String, String)
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

-------------------------------  HELPER PARSERS  ------------------------------

backtickString :: Parser String
backtickString = some $ char '`'

orderedListMarker :: Parser String
orderedListMarker = repeatBetween 1 9 digit <* choice (char <$> ".)")

unorderedListMarker :: Parser Char
unorderedListMarker = choice $ char <$> "-+*"

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

blankLine :: Parser String
blankLine = liftA2 (++) (many $ oneOf " \t") eolf
