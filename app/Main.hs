module Main where

import Lib
import Lexer

-- Wrapper for making String an instance of PrettyPrintable
newtype Wrapper = Wrapper String

-- Typeclass that has function `ppList` for showing cleaner a list of Tokens, Errors, etc.
class PrettyPrintable a where
    ppList :: [a] -> String

instance PrettyPrintable Wrapper where
    ppList [] = ""
    ppList ((Wrapper s):ss) = s ++ "\n" ++ ppList ss

instance PrettyPrintable Token where
    ppList [] = ""
    ppList (x:xs) = show x ++ "\n" ++ ppList xs


-- Main function. Currently it is only testing the lexer.
main :: IO ()
main = do
    srcFile <- getContents
    let lexResult = lexAnalysis srcFile in (
        case lexResult of
            Left errors -> do
                putStrLn $ "Something happened!\n" ++ (ppList $ map (\e -> Wrapper e) errors)
            Right matches -> do
                putStrLn $ "Tokens:\n" ++ ppList matches)