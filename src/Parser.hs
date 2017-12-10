module Parser where

import Control.Monad.State (runState)
import qualified Data.Map as M (empty)

import AST
import BlockParser (runBlockP)
import InlineParser (runInlineP)
import ParserCombinators (trim)

runMainP :: String -> [Markdown]
runMainP s = uncurry connector $ runState (runBlockP $ newlineTerminate s) M.empty
  where newlineTerminate ""       = "\n"
        newlineTerminate "\n"     = "\n"
        newlineTerminate (c : cs) = c : newlineTerminate cs

connector :: [Partial] -> LinkRefMap -> [Markdown]
connector (PUnorderedList _ tight xs : rest) m =
  UnorderedList tight ((Many . (`connector` m)) <$> xs) : connector rest m
connector (POrderedList n _ tight xs : rest) m =
  OrderedList n tight ((Many . (`connector` m)) <$> xs) : connector rest m
connector (PBlockQuote xs : rest) m =
  BlockQuote (Many $ connector xs m) : connector rest m
connector (PHeader level s : rest) m =
  Header level (Many $ runInlineP s m) : connector rest m
connector (PHorizontalRule : rest) m =
  HorizontalRule : connector rest m
connector (PCodeBlock maybeInfo s : rest) m =
  CodeBlock maybeInfo s : connector rest m
connector (PParagraph s : rest) m =
  Paragraph (Many $ runInlineP (trim s) m) : connector rest m
connector (_ : rest) m = connector rest m
connector [] _ = []
