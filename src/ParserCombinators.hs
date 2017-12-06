module ParserCombinators where

import Control.Applicative
import Data.Char (isSpace)

import Text.Parsec hiding (many, optional, (<|>))
-- import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
-- import Text.Parsec.Prim hiding (many, (<|>))

eol :: Parser String
eol =   string "\n"
    <|> liftA2 (++) (string "\r") (string "\n" <|> string "")
    <?> "end of line"

atLeast :: Int -> Parser a -> Parser [a]
atLeast n p
  | n > 0     = liftA2 (:) p $ atLeast (n - 1) p
  | otherwise = many p

atLeastN :: Int -> Parser a -> Parser Int
atLeastN n = fmap length . atLeast n

atLeast_ :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m ()
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

exactly :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
exactly n p = count n p <* betterNotFollowedBy p


-- TODO add type
betterNotFollowedBy p = (try p *> fail "not exact") <|> return ()

manyTillEnd :: Parser a -> Parser [a] -> Parser [a]
manyTillEnd p end = scan
  where scan = end <|> liftA2 (:) p scan

sepByInclusive :: Parser a -> Parser a -> Parser [a]
sepByInclusive p sep = liftA2 (:) p (concat <$> many (liftA2 twoList sep p)) <|> pure []
  where twoList x y = [x, y]

-- interleave :: Parser a -> Parser a -> Parser [a]
-- interleave p1 p2 = many


someTill :: Parser a -> Parser b -> Parser [a]
someTill p end = liftA2 (:) p (manyTill p end)


consumeUpto :: Int -> Parser a -> Parser [a]
consumeUpto 0 p = return []
consumeUpto n p = do mx <- optional p
                     case mx of
                       Just x  -> (x :) <$> consumeUpto (n - 1) p
                       Nothing -> return []
