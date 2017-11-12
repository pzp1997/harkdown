module Parser where

import Control.Applicative
import Prelude hiding (words)

import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String (Parser)

import AST
import ParserCombinators

type MdParser = Parser Markdown

-- parseBlock :: MdParser
-- parseBlock =

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
atxHeading = do atMost_ 3 $ char ' '
                hLevel <- repeatBetween 1 6 $ char '#'
                some $ () <$ spaceChar <|> () <$ eol
                content <- words
                inlineContent <- parseInline content
                optional $ some spaceChar *> some (char '#') *> some spaceChar
                return $ Header hLevel inlineContent

spaceChar :: Parser Char
spaceChar = char ' '

words :: Parser String
words = many $ noneOf "\n\r"

line :: Parser String
line = words <* eolf

blankLine :: Parser String
blankLine = many (oneOf " \t") <* eolf

backtickString :: Parser String
backtickString = some $ char '`'

-- code :: Parser String
-- code = do start <- backtickString
--           let level = length start
--           end <- backtickString

eol :: Parser Char
eol = try crlf
    <|> newline
    <|> char '\r'
    <?> "end of line"

eolf :: Parser ()
eolf = () <$ eol <|> eof
