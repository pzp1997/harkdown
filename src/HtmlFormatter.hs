module HtmlFormatter where

import Data.Maybe (maybe)

import Text.PrettyPrint

import AST
import Parser (mainP)

type Attributes = [(String, String)]

markdownToHtml :: String -> String
markdownToHtml = renderHtml . mainP

renderHtml :: [Markdown] -> String
renderHtml = render . foldMap htmlify

htmlify :: Markdown -> Doc
htmlify (Text s)              = text s
htmlify (Bold md)             = tag "strong" [] $ htmlify md
htmlify (Italics md)          = tag "em" [] $ htmlify md
htmlify (Header level md)     = nl $ tag ("h" ++ show level) [] $ htmlify md
htmlify (Link href mTitle md) = tag "a" attr $ htmlify md
  where attr = case mTitle of
                 Just title -> [("href", href), ("title", title)]
                 Nothing    -> [("href", href)]
htmlify (Image src mTitle md) =
  selfClosingTag "img" $ [("src", src), ("alt", concat $ extractText md)] ++
                         maybe [] (\title -> [("title", title)]) mTitle
htmlify (Paragraph md)        = nl $ tag "p" [] $ htmlify md
htmlify (OrderedList n tight items) = nlTag "ol" attr $ listItems tight items
  where attr = if n == 1 then [] else [("start", show n)]
htmlify (UnorderedList tight items) = nlTag "ul" [] $ listItems tight items
htmlify (BlockQuote xs)       = nlTag "blockquote" [] $ htmlify xs
htmlify (CodeBlock info s)    = nl $ tag "pre" [] $ tag "code" codeAttr $ text s
  where codeAttr = if null info then [] else [("class", "language-" ++ info)]
htmlify (Code s)     = tag "code" [] $ text s
htmlify HorizontalRule        = nl $ selfClosingTag "hr" []
htmlify SoftBreak             = char '\n'
htmlify HardBreak             = selfClosingTag "br" []
htmlify (Many xs)             = foldMap htmlify xs

tag :: String -> Attributes -> Doc -> Doc
tag tagName attr contents = text ("<" ++ tagName ++ strOfAttr attr ++ ">") <>
                              contents <> text ("</" ++ tagName ++ ">")

nlTag :: String -> Attributes -> Doc -> Doc
nlTag tagName attr contents =  text ("<" ++ tagName ++ strOfAttr attr ++ ">")
                            <> char '\n'
                            <> contents <> text ("</" ++ tagName ++ ">")
                            <> char '\n'

selfClosingTag :: String -> Attributes -> Doc
selfClosingTag tagName attr = text ("<" ++ tagName ++ strOfAttr attr ++ " />")

strOfAttr :: Attributes -> String
strOfAttr = concatMap keyValueStr
  where keyValueStr (k, v) = concat [" ", k, "=\"", v, "\""]

listItems :: Bool -> [Markdown] -> Doc
listItems tight xs
  | tight && all singleton xs = foldMap (nl . tag "li" [] . htmlify . tightContent) xs
  | otherwise                 = foldMap (nlTag "li" [] . htmlify) xs
  where singleton (Many [_]) = True
        singleton (Many _)   = False
        singleton _          = True
        tightContent (Many [md]) = md
        tightContent (Many _)    = undefined
        tightContent md          = md

nl :: Doc -> Doc
nl = (<> char '\n')

extractText :: Markdown -> [String]
extractText (Text s)                = [s]
extractText (Bold md)               = extractText md
extractText (Italics md)            = extractText md
extractText (Header _ md)           = extractText md
extractText (Link _ _ md)           = extractText md
extractText (Image _ _ md)          = extractText md
extractText (Paragraph md)          = extractText md
extractText (OrderedList _ _ items) = concatMap extractText items
extractText (UnorderedList _ items) = concatMap extractText items
extractText (BlockQuote xs)         = extractText xs
extractText (CodeBlock _ s)         = [s]
extractText (Code s)                = [s]
extractText HorizontalRule          = []
extractText SoftBreak               = []
extractText HardBreak               = []
extractText (Many xs)               = concatMap extractText xs
