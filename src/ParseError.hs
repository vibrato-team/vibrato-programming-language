module ParseError where
import Lexer
import Tokens

parseError :: Token -> Alex a
parseError tk = case tk of
    ErrorToken str l c -> alexError $ "Lexical error at line " ++ show l ++ ", column " ++ show c ++ ": " ++ show str
    _ -> alexError $ "Parser error at line " ++ show (line tk) ++ ", column " ++ show (col tk) ++ ": " ++ show (token tk)