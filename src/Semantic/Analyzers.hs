module Semantic.Analyzers where
import Frontend.Tokens
import qualified AST
import Frontend.Parser.Monad (ParserMonad)
import Frontend.Parser.Monad as PMonad
import Data.Maybe
import qualified Control.Monad.RWS.Lazy as RWS
import Control.Monad.Trans
import Util.Error
import Control.Monad
import Data.List

-- | Throws a semantic error
semError :: Token -> String -> ParserMonad a
semError tk msg = do
    srcFile <- RWS.ask
    throwCompilerError srcFile [Error (line tk) (col tk) msg]
   
-- | Throw a semantic error if Maybe is Nothing
throwIfNothing :: Maybe a -> Token -> String ->  ParserMonad ()
throwIfNothing Nothing tk msg = Semantic.Analyzers.pushError tk msg
throwIfNothing (Just _) _ _ = return ()

-----------------------------------------------------------------------------------------------
-- Semantic analysers
-----------------------------------------------------------------------------------------------

-- | Checks if variable is declared, throws an error if it is not
checkVarIsDeclared :: Token -> ParserMonad (Maybe AST.Entry)
checkVarIsDeclared tk = do
    let tkString = token tk
    entryMaybe <- PMonad.lookup tkString
    throwIfNothing entryMaybe tk "Identifier not in scope:"
    return entryMaybe

-- Chequea que un lvalue no sea const
checkConstLvalue :: AST.Expression -> ParserMonad ()
checkConstLvalue (AST.IdExp id _ _) = do
    let tkString = token $ AST.id_token id
    entryMaybe <- PMonad.lookup tkString
    case entryMaybe of
        Just (AST.Entry _ (AST.Const _) _ _ _) -> Semantic.Analyzers.pushError (AST.id_token id) "You cannot modify a Const:"
        Just _ -> return ()
        Nothing -> return ()
checkConstLvalue _ = return ()

-- | Analizes if field correspond to the type of the entry and throws an error if it is not
analyzeField :: Token -> Int -> ParserMonad AST.Entry
analyzeField tk level = do
    let tkString = token tk
    entryMaybe <- PMonad.lookupField tkString level

    throwIfNothing entryMaybe tk (show tkString ++ " is not a valid field:")
    return $ fromJust entryMaybe

matchingError tkString = "Couldnt match closing " ++ tkString ++ ":"

pushIfError :: Bool -> Token -> String -> ParserMonad ()
pushIfError good tk msg =
    Control.Monad.unless good $ PMonad.pushError $ Error (line tk) (col tk) msg

pushError :: Token -> String -> ParserMonad ()
pushError = pushIfError False

--------------------------------------------
-----------Chequear tipos ------------------
--------------------------------------------

-- Chequea que ambos tipos sean compatibles
equalType :: AST.ASTType -> AST.ASTType -> Bool
equalType (AST.Compound ou inner) (AST.Compound ou' inner') =
    ou == ou' && equalType inner inner'

equalType _ (AST.Simple "Error") = True
equalType (AST.Simple "Error") _ = True
    
equalType (AST.Simple ou) (AST.Simple ou') =
    ou == ou'

equalType (AST.Compound "Melody" _) (AST.Simple "empty_list") = True
equalType (AST.Simple "empty_list") (AST.Compound "Melody" _) = True

equalType (AST.Compound "Sample" _) (AST.Simple "null") = True
equalType (AST.Simple "null") (AST.Compound "Sample" _) = True

equalType _ _ = False

-- | Chequea que el tipo de la expresión sea compatible con alguno de los tipos pasados como argumento.
checkExpType :: AST.Expression -> [AST.ASTType] -> Token -> ParserMonad AST.ASTType
checkExpType exp expected tk = do
    let expType = AST.exp_type exp
    Control.Monad.unless (any (equalType expType) expected) $ Semantic.Analyzers.pushError tk $ "Expression isn't of type " ++ showExpected expected ++ ":"
    return expType

checkLValueIsAllocated :: AST.Expression -> Token -> ParserMonad ()
checkLValueIsAllocated exp tk = do
    let expType = AST.exp_type exp
    case expType of
        AST.Compound _ _ -> return ()
        _ -> Semantic.Analyzers.pushError tk "Expression isn't a Sample or a Melody:"

-- | Para imprimir bonito los tipos esperados
showExpected :: [AST.ASTType] -> String
showExpected [] = ""
showExpected [t] = show t
showExpected expected@(_:_:_) = let strs = map show expected in
    intercalate ", " (init strs) ++ " or " ++ (last strs)

-- | Chequea que la expresión derecha sea compatible para ser asignada con la expresión izquierda
checkAssignment :: AST.Expression -> AST.Expression -> Token -> ParserMonad ()
checkAssignment = checkAssignment' . AST.exp_type

-- | Chequea que el tipo de la expresión pueda ser asignado al tipo pasado por parámetro.
checkAssignment' :: AST.ASTType -> AST.Expression -> Token -> ParserMonad ()
checkAssignment' astType exp tk = do
    if astType `elem` AST.numberTypes
        then checkExpType exp (filter (<= astType) AST.numberTypes) tk
        else checkExpType exp [astType] tk
    return ()

-- | Chequea que los tipos sean comparables
checkEquality :: AST.Expression -> AST.Expression -> Token -> ParserMonad ()
checkEquality lexp rexp tk = do
    let astType = AST.exp_type lexp
    if astType `elem` AST.numberTypes
        then checkExpType rexp AST.numberTypes tk
        else checkExpType rexp [astType] tk
    return ()

-- | Retorna la expresión si es del tipo escogido, si no, retorna la expresión casteada.
castExp :: AST.Expression -> AST.ASTType -> AST.Expression
castExp exp finalType =
    if AST.exp_type exp == finalType 
        then exp
        else AST.CastExp exp (AST.exp_type exp) finalType

-- | Chequear que los parametros tengan tipos compatibles con los argumentos y retorna una lista
-- de de los parametros casteados si no hubo un error.
checkParams :: Token -> [AST.Expression] -> [AST.VarDeclaration] -> ParserMonad [AST.Expression]
checkParams id xs ys = checkParams' id xs ys []

checkParams' :: Token -> [AST.Expression] -> [AST.VarDeclaration] -> [AST.Expression] -> ParserMonad [AST.Expression]
checkParams' id [] [] lst = return lst

checkParams' id xs [] _ = do
    Semantic.Analyzers.pushError id "Too much arguments:"
    return [] 

checkParams' id [] ys lst = do
    Semantic.Analyzers.pushError id "Too few arguments:"
    return []

checkParams' id (x:xs) (y:ys) lst = do
    let ytype = AST.var_type y
    checkAssignment' ytype x id
    checkParams' id xs ys ((castExp x ytype):lst)

notDeclaration = \inst -> case inst of 
    AST.VarDecInst _ -> False 
    _ -> True