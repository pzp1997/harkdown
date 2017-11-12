module ParserCombinators where

import Control.Applicative

import Text.Parsec (skipMany)
import Text.Parsec.String (Parser)

atLeast :: Int -> Parser a -> Parser [a]
atLeast n p
  | n > 0     = liftA2 (:) p $ atLeast (n - 1) p
  | otherwise = many p

atMost :: Int -> Parser a -> Parser [a]
atMost n p
  | n >= 0    = liftA2 (:) p (atMost (n - 1) p) <|> return []
  | otherwise = fail "too many"

atLeast_ :: Int -> Parser a -> Parser ()
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
