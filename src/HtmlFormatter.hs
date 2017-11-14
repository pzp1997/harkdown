module HtmlFormatter where

import Text.PrettyPrint

import AST

type Attributes = [(String, String)]

renderHtml :: [Markdown] -> String
renderHtml = render . foldMap htmlify

htmlify :: Markdown -> Doc
htmlify (Text s)              = text s
htmlify (Bold md)             = tag "strong" [] $ htmlify md
htmlify (Italics md)          = tag "em" [] $ htmlify md
htmlify (Header level md)     = tag ("h" ++ show level) [] $ htmlify md
htmlify (Link href md)        = tag "a" [("href", href)] $ htmlify md
htmlify (Image src md)        =
  selfClosingTag "img" [("src", src), ("alt", concat $ extractText md)]
htmlify (Paragraph md)        = tag "p" [] $ htmlify md
htmlify (OrderedList items)   = tag "ol" [] $ listItems items
htmlify (UnorderedList items) = tag "ul" [] $ listItems items
htmlify (BlockQuote xs)       = tag "blockquote" [] $ foldMap htmlify xs
htmlify (BlockLiteral info s) = tag "pre" [] $ tag "code" codeAttr $ text s
  where codeAttr = case info of
                     Just infoString -> [("class", "language-" ++ infoString)]
                     Nothing         -> []
htmlify (InlineLiteral s)     = tag "code" [] $ text s
htmlify HorizontalRule        = selfClosingTag "hr" []

tag :: String -> Attributes -> Doc -> Doc
tag tagName attr contents = text ("<" ++ tagName ++ strOfAttr attr ++ ">") <>
                              contents <> text ("</" ++ tagName ++ ">")

selfClosingTag :: String -> Attributes -> Doc
selfClosingTag tagName attr = text ("<" ++ tagName ++ strOfAttr attr ++ " />")

strOfAttr :: Attributes -> String
strOfAttr = concatMap keyValueStr
  where keyValueStr (k, v) = concat [" ", k, "=\"", v, "\""]

listItems :: [Markdown] -> Doc
listItems = foldMap $ tag "li" [] . htmlify

extractText :: Markdown -> [String]
extractText (Text s)              = [s]
extractText (Bold md)             = extractText md
extractText (Italics md)          = extractText md
extractText (Header _ md)         = extractText md
extractText (Link _ md)           = extractText md
extractText (Image _ md)          = extractText md
extractText (Paragraph md)        = extractText md
extractText (OrderedList items)   = concatMap extractText items
extractText (UnorderedList items) = concatMap extractText items
extractText (BlockQuote xs)       = concatMap extractText xs
extractText (BlockLiteral _ s)    = [s]
extractText (InlineLiteral s)     = [s]
extractText HorizontalRule        = []
