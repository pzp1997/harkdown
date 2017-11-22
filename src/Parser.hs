module Parser where

import Control.Applicative
import Control.Monad
import Data.List (isPrefixOf)
import Prelude hiding (words, lines)

import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)

import AST
import ParserCombinators

type MdParser = Parser Markdown

-- parseBlock :: MdParser
-- parseBlock =
parseMarkdown :: String -> [Markdown]
parseMarkdown = undefined

parseInline :: String -> MdParser
parseInline = return . Text

-- TODO is ' ' the only "space" character or any non newline whitespace?
thematicBreak :: MdParser
thematicBreak = atMost_ 3 spaceChar
             *> choice (atLeast_ 3 . breakChar <$> "*-_")
             *> eol
             *> pure HorizontalRule
  where breakChar c = char c <* many spaceChar

atxHeading :: MdParser
atxHeading = do atMost_ 3 spaceChar
                hLevel <- repeatBetween 1 6 hashtagChar
                content <- some spaceChar *> manyTill (noneOf "\n\r")
                  (try $ spacesAround (many hashtagChar) *> eol) <|> "" <$ eol
                inlineContent <- parseInline content
                return $ Header hLevel inlineContent
  where hashtagChar = char '#'

fencedCode :: MdParser
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

blockquote :: MdParser
blockquote = BlockQuote <$> some $ atMost_ 3 spaceChar *> char '>' *>
                            optional (char ' ') *> parseBlock

orderedListMarker :: Parser String
orderedListMarker = repeatBetween 1 9 (satisfies isDigit) <* choice ".)"

unorderedListMarker :: Parser Char
unorderedListMarker = choice "-+*"

words :: Parser String
words = many $ noneOf "\n\r"

line :: Parser String
line = words <* eolf

lines :: Parser [String]
lines = endBy words eolf

blankLine :: Parser String
blankLine = many (oneOf " \t") <* eolf

backtickString :: Parser String
backtickString = some $ char '`'

-- code :: Parser String
-- code = do start <- backtickString
--           let level = length start
--           end <- backtickString

eol :: Parser String
eol =   string "\n"
    <|> liftA2 (++) (string "\r") (string "\n" <|> string "")
    <?> "end of line"

eolf :: Parser ()
eolf = () <$ eol <|> eof
