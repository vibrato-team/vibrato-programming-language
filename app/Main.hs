module Main where

import Lib
import Lexer

newtype Wrapper = Wrapper String

class PrettyPrintable a where
    ppList :: [a] -> String

instance PrettyPrintable Wrapper where
    ppList [] = ""
    ppList ((Wrapper s):ss) = s ++ "\n" ++ ppList ss

instance PrettyPrintable Token where
    ppList [] = ""
    ppList (x:xs) = show x ++ "\n" ++ ppList xs

main :: IO ()
main = do
    srcFile <- getContents
    let lexResult = lexAnalysis srcFile in (
        case lexResult of
            Left errors -> do
                putStrLn $ "Something happened!\n" ++ (ppList $ map (\e -> Wrapper e) errors)
            Right matches -> do
                putStrLn $ "Tokens:\n" ++ ppList matches)