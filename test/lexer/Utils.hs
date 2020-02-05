module Utils where
import Frontend.Lexer
import Frontend.Tokens

getTok :: Either String AlexUserState -> [Token]
getTok lexTok = 
    case lexTok of
        Left _ -> error "Test Failed"
        Right state -> 
            case (matches state) of
                Left _ -> error "Test Failed"
                Right tokens -> tokens

ppList ::  [Token] -> String
ppList [] = ""
ppList [x] = token x
ppList (x:xs) = token x ++ " " ++ ppList xs
