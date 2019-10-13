module Util.Error where

import Data.List.Split
import Tokens

showError :: String -> String -> Int -> Int -> String
showError src msg ln col =
    "\x1b[1m" ++ errorMessage ++ "\x1b[0m" ++ line ++ "\n" ++ replicate (col-1) ' ' ++ errorPointer
    where
        errorMessage = show ln ++ ":" ++ show col ++ ": " ++ msg ++ "\n"
        line = splitOn "\n" src !! (ln-1)
        errorPointer = "\x1b[1;31m^\x1b[0m"

showErrors srcFile = map (\err -> showError srcFile (errMsg err) (errLine err) (errCol err))

-- Lexical error
data Error = Error { errLine :: Int, errCol :: Int, errMsg :: String } deriving (Show)

throwCompilerError srcFile errors =
    error $ "\n" ++ unlines (showErrors srcFile errors)