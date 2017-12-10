module Main where

import Control.Monad (unless)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

import HtmlFormatter (renderHtml)
import Parser (runMainP)

main :: IO ()
main = do
  args <- getArgs
  unless (null args) $ do
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    putStrLn $ renderHtml $ runMainP contents
    hClose handle
