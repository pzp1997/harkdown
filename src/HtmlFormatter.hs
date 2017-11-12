module HtmlFormatter where

import Text.PrettyPrint

import AST

renderHtml :: [Markdown] -> String
renderHtml = render . foldMap htmlify

tag :: String -> Doc -> Doc
tag tagName contents =
  text ("<" ++ tagName ++ ">") <> contents <> text ("</" ++ tagName ++ ">")

listItems :: [Markdown] -> Doc
listItems = foldMap $ tag "li" . htmlify

htmlify :: Markdown -> Doc
htmlify (Text s)           = text s
htmlify (Bold md)          = tag "strong" $ htmlify md
htmlify (Italics md)       = tag "em" $ htmlify md
htmlify (Header level md)  = tag ("h" ++ show level) $ htmlify md
htmlify (Link href md)     =
  text ("<a href=\"" ++ href ++ "\">") <> htmlify md <> text "</a>"
htmlify (Image href md)    =
  text ("<img href=\"" ++ href ++ "\" alt=\"") <> htmlify md <> text "\">"
htmlify (Paragraph md)     = tag "p" $ htmlify md
htmlify (OrderedList xs)   = tag "ol" $ listItems xs
htmlify (UnorderedList xs) = tag "ul" $ listItems xs
htmlify (BlockQuote xs)    = tag "blockquote" $ foldMap htmlify xs
htmlify (BlockLiteral s)   = tag "pre" $ tag "code" $ text s
htmlify (InlineLiteral s)  = tag "code" $ text s
htmlify HorizontalRule     = text "<hr>"
