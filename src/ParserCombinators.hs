module ParserCombinators where

import Control.Applicative
import Data.Char (isSpace)

import Text.Parsec (satisfy, char, skipMany, between
  , notFollowedBy, manyTill, try, lookAhead, Parsec)
import Text.Parsec.String (Parser)

atLeast :: Int -> Parser a -> Parser [a]
atLeast n p
  | n > 0     = liftA2 (:) p $ atLeast (n - 1) p
  | otherwise = many p

atMost :: Int -> Parser a -> Parser [a]
atMost n p
  | n >= 0    = liftA2 (:) p (atMost (n - 1) p) <|> return []
  | otherwise = fail "too many"

atLeast_ :: Int -> Parsec s () a -> Parsec s () ()
atLeast_ n p
  | n > 0     = p *> atLeast_ (n - 1) p
  | otherwise = skipMany p

atMost_ :: Int -> Parser a -> Parser ()
atMost_ n p
  | n >= 0    = (p *> atMost_ (n - 1) p) <|> return ()
  | otherwise = fail "too many"

repeatBetween :: Int -> Int -> Parser a -> Parser Int
repeatBetween lo hi p = helper 0
  where helper n
          | n < lo    = p *> helper (n + 1)
          | n <= hi   = (p *> helper (n + 1)) <|> return n
          | otherwise = fail "too many"

spaceChar :: Parser Char
spaceChar = char ' ' -- might include tabs for code blocks?

nonWhiteSpace :: Parser Char
nonWhiteSpace = satisfy $ not . isSpace


spacesAround :: Parser a -> Parser a
spacesAround = between (many spaceChar) (many spaceChar)

-- manyTill :: Parser a -> Parser b -> Parser [a]
-- manyTill p end = (try end *> return []) <|> liftA2 (:) p manyTill

{-
-- | Variant of manyTill that only succeeds if the first parser succeeds at
--   least once.
many1Till :: (Show a, Show b) => Parser a -> Parser b -> Parser[a]
many1Till p end = do
  notFollowedBy end
  aVal <- p
  rest <- manyTill p end
  return $ aVal : rest
-}
