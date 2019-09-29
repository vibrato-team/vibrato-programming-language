module Main where

import Lib
import Lexer

main :: IO ()
main = do
    srcFile <- getContents
    putStrLn $ show $ alexScanTokens srcFile