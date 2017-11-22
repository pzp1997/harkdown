module Parser where

import Control.Applicative
import Control.Monad
import Data.List (isPrefixOf)
import Prelude hiding (words, lines)

import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)

import AST
import ParserCombinators

--------------------------------  BIG PARSERS  --------------------------------

parseMarkdown :: String -> [Markdown]
parseMarkdown input = case parse (many atxHeading) "" input of
                        Left  _  -> []
                        Right md -> md

parseBlock :: String -> [Partial]
parseBlock = undefined

parseInline :: [Partial] -> [Markdown]
parseInline = return . Text

----------------------------  BLOCK LEVEL PARSERS  ----------------------------

thematicBreak :: Parser Markdown
thematicBreak =  atMost_ 3 spaceChar
              *> choice (atLeast_ 3 . breakChar <$> "*-_")
              *> eol
              *> pure HorizontalRule
  where breakChar c = char c <* many spaceChar

atxHeading :: Parser Markdown
atxHeading = do atMost_ 3 spaceChar
                hLevel <- repeatBetween 1 6 hashtagChar
                content <- some spaceChar *> manyTill (noneOf "\n\r")
                  (try $ spacesAround (many hashtagChar) *> eol) <|> "" <$ eol
                inlineContent <- parseInline content
                return $ Header hLevel inlineContent
  where hashtagChar = char '#'

fencedCode :: Parser Markdown
fencedCode = do indent <- atMost 3 spaceChar
                let indentSize = length indent
                openFence <- fence
                infoString <- optionMaybe $ spacesAround $ some nonWhiteSpace
                content <- manyTill (atMost indentSize spaceChar *> litLine) $
                  eof <|> try (do closeFence <- fence
                                  unless (openFence `isPrefixOf` closeFence) $
                                    parserFail "closing code fence")
                return $ BlockLiteral infoString $ concat content
  where fence = choice $ atLeast 3 . char <$> "`~"
        litLine = liftA2 (++) words eol

blockquote :: Parser Markdown
blockquote = BlockQuote <$> some $ atMost_ 3 spaceChar *> char '>' *>
                            optional (char ' ') *> parseBlock


----------------------------  INLINE LEVEL PARSERS  ---------------------------


-------------------------------  HELPER PARSERS  ------------------------------

backtickString :: Parser String
backtickString = some $ char '`'

orderedListMarker :: Parser String
orderedListMarker = repeatBetween 1 9 (satisfies isDigit) <* choice ".)"

unorderedListMarker :: Parser Char
unorderedListMarker = choice "-+*"

----------------------------  DEFINITIONAL PARSERS  ---------------------------

eol :: Parser String
eol =   string "\n"
    <|> liftA2 (++) (string "\r") (string "\n" <|> string "")
    <?> "end of line"

eolf :: Parser ()
eolf = () <$ eol <|> eof

words :: Parser String
words = many $ noneOf "\n\r"

line :: Parser String
line = words <* eolf

lines :: Parser [String]
lines = endBy words eolf

blankLine :: Parser String
blankLine = many (oneOf " \t") <* eolf
