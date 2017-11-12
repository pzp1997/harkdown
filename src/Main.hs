module Main where

import Control.Applicative

import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Char
import Text.Parsec.String (Parser)

import AST

type MdParser = Parser Markdown

main :: IO ()
main = return ()

-- atLeast :: Int -> Parser a -> Parser [a]
-- atLeast n p
--   | n > 0     = liftA2 (:) p $ atLeast (n - 1) p
--   | otherwise = many p
--
-- atMost :: Show a => Int -> Parser a -> Parser [a]
-- atMost n p = helper n <* notFollowedBy p
--   where helper n
--           | n > 0     = liftA2 (:) p (helper $ n - 1) <|> return []
--           | otherwise = return []

atLeast_ :: Int -> Parser a -> Parser ()
atLeast_ n p
  | n > 0     = p >> atLeast_ (n - 1) p
  | otherwise = skipMany p

atMost_ :: Show a => Int -> Parser a -> Parser ()
atMost_ n p = helper n <* notFollowedBy p
  where helper n
          | n > 0     = (p >> helper (n - 1)) <|> return ()
          | otherwise = return ()

thematicBreak :: MdParser
thematicBreak = atMost 3 sp
             *> choice (atLeast 3 . breakChar <$> "*-_")
             *> eol
             *> pure HorizontalRule
  where sp = char ' ' -- TODO is this only space character or any non newline whitespace?
        breakChar c = char c <* many sp

line :: Parser String
line = many (noneOf "\n\r") <* eol

blankLine :: Parser String
blankLine = many (oneOf " \t") <* eol

backtickString :: Parser String
backtickString = some $ char '`'

-- atMost2 :: Parser a -> Parser [a]
-- atMost2 p = choice [ (++) <$> singleP <*> singleP
--                    , singleP
--                    , return []
--                    ] <* eof
--     where singleP = pure <$> p

-- atMost n p = count n (p <|> return []) <* notFollowedBy p


-- code :: Parser String
-- code = do start <- backtickString
--           let level = length start
--           end <- backtickString

eol :: Parser Char
eol = try crlf
    <|> newline
    <|> char '\r'
    <?> "end of line"
