module HorizontalRule where

import Test.HUnit (Test(TestList), (~?=))
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

import Parser (thematicBreak)
import AST (Markdown(HorizontalRule))

tParse :: (Eq a, Show a) => Parser a -> a -> String -> Test
tParse p e s = parse p "" s ~?= Right e

tHorizontalRule :: Test
tHorizontalRule = TestList $ tParse thematicBreak HorizontalRule <$>
  [ "***\n"
  , "---\n"
  , "___\n"
  , " ***\n"
  , "  ***\n"
  , "   ***\n"
  , "_____________________________________\n"
  , " - - -\n"
  , " **  * ** * ** * **\n"
  , "-     -      -      -\n"
  , "- - - -    \n"
  ]

-- tooFewBreakChars = TestList
--   [ "--"
--   , "**"
--   , "__"
--   ]
--
-- wrongBreakChars = TestList
--   [ "+++"
--   , "==="
--   ]
--
-- tooManySpaces = "    ***"
--
--
-- noOtherChars = TestList
--   [ "_ _ _ _ a"
--   , "a------"
--   , "---a---"
--   ]
