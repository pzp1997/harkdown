module ParserCombinators where

import Control.Applicative
import Data.Char (isSpace)

import Text.Parsec (satisfy, char, skipMany, between)
import Text.Parsec.String (Parser)

atLeast :: Int -> Parser a -> Parser [a]
atLeast n p
  | n > 0     = liftA2 (:) p $ atLeast (n - 1) p
  | otherwise = many p

atLeast_ :: Int -> Parser a -> Parser ()
atLeast_ n p
  | n > 0     = p *> atLeast_ (n - 1) p
  | otherwise = skipMany p

atMost :: Int -> Parser a -> Parser [a]
atMost n p
  | n >= 0    = liftA2 (:) p (atMost (n - 1) p) <|> return []
  | otherwise = fail "too many"

atMostN :: Int -> Parser a -> Parser Int
atMostN n = fmap length . atMost n

atMost_ :: Int -> Parser a -> Parser ()
atMost_ n p
  | n >= 0    = (p *> atMost_ (n - 1) p) <|> return ()
  | otherwise = fail "too many"

repeatBetweenN :: Int -> Int -> Parser a -> Parser Int
repeatBetweenN lo hi p = helper 0
  where helper n
          | n < lo    = p *> helper (n + 1)
          | n <= hi   = (p *> helper (n + 1)) <|> return n
          | otherwise = fail "too many"

repeatBetween :: Int -> Int -> Parser a -> Parser [a]
repeatBetween lo hi p = helper 0
  where helper n
          | n < lo    = consP n
          | n <= hi   = consP n <|> return []
          | otherwise = fail "too many"
        consP n = liftA2 (:) p (helper $ n + 1)

spaceChar :: Parser Char
spaceChar = char ' ' -- might include tabs for code blocks?

nonWhiteSpace :: Parser Char
nonWhiteSpace = satisfy $ not . isSpace

spacesAround :: Parser a -> Parser a
spacesAround = between (many spaceChar) (many spaceChar)

-- manyTill :: Parser a -> Parser b -> Parser [a]
-- manyTill p end = (try end *> return []) <|> liftA2 (:) p manyTill
