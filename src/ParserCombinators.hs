module ParserCombinators where

import Control.Applicative
import Data.Char (isSpace)

import Text.Parsec hiding (many, optional, (<|>))

eol :: Parsec String u String
eol =   string "\n"
    <|> liftA2 (++) (string "\r") (string "\n" <|> string "")
    <?> "end of line"

atLeast :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
atLeast n p
  | n > 0     = liftA2 (:) p $ atLeast (n - 1) p
  | otherwise = many p

atMost :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
atMost n p
  | n >= 0    = liftA2 (:) p (atMost (n - 1) p) <|> return []
  | otherwise = fail "too many"

repeatBetweenN :: Stream s m t => Int -> Int -> ParsecT s u m a -> ParsecT s u m Int
repeatBetweenN lo hi = (length <$>) . repeatBetween lo hi

repeatBetween :: Stream s m t => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
repeatBetween lo hi p = helper 0
  where helper n
          | n < lo    = consP n
          | n <= hi   = consP n <|> return []
          | otherwise = fail "too many"
        consP n = liftA2 (:) p (helper $ n + 1)

spaceChar :: Parsec String u Char
spaceChar = char ' ' -- might include tabs for code blocks?

nonWhiteSpace :: Parsec String u Char
nonWhiteSpace = satisfy $ not . isSpace

spacesAround :: Parsec String u a -> Parsec String u a
spacesAround = between (many spaceChar) (many spaceChar)

exactly :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
exactly n p = count n p <* betterNotFollowedBy p

betterNotFollowedBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
betterNotFollowedBy p = (try p *> fail "not exact") <|> return ()

manyTillEnd :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a] -> ParsecT s u m [a]
manyTillEnd p end = scan
  where scan = end <|> liftA2 (:) p scan

sepByInclusive :: Stream s m t => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m [a]
sepByInclusive p sep = liftA2 (:) p (concat <$> many (liftA2 twoList sep p)) <|> pure []
  where twoList x y = [x, y]

someTill :: Stream s m t => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
someTill p end = liftA2 (:) p (manyTill p end)

consumeUpto :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
consumeUpto 0 _ = return []
consumeUpto n p = do mx <- optional p
                     case mx of
                       Just x  -> (x :) <$> consumeUpto (n - 1) p
                       Nothing -> return []
