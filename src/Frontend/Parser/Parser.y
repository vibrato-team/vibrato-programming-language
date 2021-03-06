{
module Frontend.Parser.Parser(parse, voidType, createFunctionEntry, createParamEntry, createTypeEntry, parseError, PMonad.ParserMonad) where
import Frontend.Lexer
import Frontend.Tokens
import qualified AST
import Util.Error
import Data.Either
import Data.Maybe
import Frontend.Parser.Monad (ParserMonad)
import qualified Frontend.Parser.Monad as PMonad
import qualified Control.Monad.RWS.Lazy as RWS
import Control.Monad.Trans
import Semantic.Analyzers
import Data.List
import Control.Monad
}

%name parse
%error { parseError }
%tokentype { Token }
%monad { PMonad.ParserMonad }

%token
    whole               { WholeToken _ _ _ }
    half                { HalfToken _ _ _ }
    quarter             { QuarterToken _ _ _ }
    eighth              { EightToken _ _ _ }
    melody              { MelodyToken _ _ _ }
    sample              { SampleToken _ _ _ }
    ThirtySecond        { ThirtySecondToken _ _ _ }
    SixtyFourth         { SixtyFourthToken _ _ _ }

    TT                  { TTToken _ _ _ }

    '<->'               { AssignToken _ _ _ }
    '{'                 { OpenCurlyToken _ _ _ }
    '}'                 { CloseCurlyToken _ _ _ }
    '|'                 { BarToken _ _ _ }
    
    '('                 { OpenParToken _ _ _ }
    ')'                 { CloseParToken _ _ _ }
    '@'                 { RecordToken _ _ _ }
    '|>'                { PlaySymToken _ _ _ }

    if                  { IfToken _ _ _ }
    else                { ElseToken _ _ _ }

    loop                { LoopToken _ _ _ }
    ':'                 { ColonToken _ _ _ }
    in                  { InToken _ _ _ }
    ','                 { CommaToken _ _ _ }

    '>>'                { NextToken _ _ _ }
    '|]'                { StopToken _ _ _ }

    '#'                 { SharpToken _ _ _ }
    '&'                 { FlatToken _ _ _ }

    track               { TrackToken _ _ _ }
    '||'                { DoubleBarToken _ _ _ }
    play                { PlayToken _ _ _ }
    with                { WithToken _ _ _ }

    new                 { NewToken _ _ _ }
    free                { FreeToken _ _ _ }

    chord               { ChordToken _ _ _ }
    legato              { LegatoToken _ _ _ }

    '!'                 { DereferenceToken _ _ _ }

    not                 { NotToken _ _ _ }
    and                 { AndToken _ _ _ }
    or                  { OrToken _ _ _ }

    '-'                 { MinusToken _ _ _ }
    mod                 { ModToken _ _ _ }
    '/'                 { DivToken _ _ _ }
    '*'                 { MultToken _ _ _ }
    '^'                 { PowToken _ _ _ }
    '+'                 { PlusToken _ _ _ }

    '='                 { EqualToken _ _ _ }
    '/='                { NotEqualToken _ _ _ }
    '<'                 { LessToken _ _ _ }
    '>'                 { GreaterToken _ _ _ }
    '<='                { LessEqualToken _ _ _ }
    '>='                { GreaterEqualToken _ _ _ }

    '['                 { BracketOpenToken _ _ _ }
    ']'                 { BracketCloseToken _ _ _ }

    '.'                 { DotToken _ _ _ }

    length              { LengthToken _ _ _ }

    int                 { IntToken _ _ _ }
    float               { FloatToken _ _ _ }
    maj                 { MajToken _ _ _ }
    min                 { MinToken _ _ _ }
    string              { StringToken _ _ _ }
    char                { CharToken _ _ _ }

    id                  { IdToken _ _ _ }
    id_type             { IdTypeToken _ _ _ }

    main                { MainToken _ _ _ }

%nonassoc play
%nonassoc loop
%nonassoc '<->'
%left and or
%right not
%nonassoc '=' '/=' 
%nonassoc '>' '<' '<=' '>='
%left LVALUE
%left '+' '-'
%left '*' '/' mod
%left '^'
%left NEG '#' '&'
%right '['
%left ']'
%left '!'
%left '.'

%nonassoc error

%%

Start                   :: { () }
Start                   : ExternalList MainDeclaration      { }

ExternalList            :: { () }
ExternalList            : ExternalList FunctionDeclaration  { }             
                        | ExternalList ChordLegato          { }
                        | {- empty -}                       { }

FunctionDeclaration     :: { () }                                           -- add block to function entry
FunctionDeclaration     : Signature Block PopScope                          {% do
                                                                                currOffset <- PMonad.getOffset
                                                                                PMonad.updateEntry (addBlock $2 currOffset) $1
                                                                                PMonad.clearReturnType }
MainDeclaration         : Main PushScope '(' ClosePar Block                 {% do
                                                                                pushIfError $4 $3 $ matchingError "parentheses"
                                                                                currOffset <- PMonad.getOffset

                                                                                let idMain = AST.Id $1 in createFunctionEntry (AST.id_token idMain) Nothing idMain [] (Just $5) (Just currOffset) }

Signature               :: { String }
Signature               : TrackId PushScope '(' ListaParam ClosePar MaybeType             {% do
                                                                                                pushIfError $5 $3 $ matchingError "parentheses"
                                                                                                let tk = AST.id_token $1
                                                                                                PMonad.addReturnType $6
                                                                                                return $ token tk }

TrackId                 :: { AST.Id }
TrackId                 : track Id                                  {% PMonad.resetOffset >> return $2 }

Main                    :: { Token }
Main                    : main                                      {% PMonad.resetOffset >> return $1 }

ListaParam              :: { [AST.VarDeclaration] }
ListaParam              : ParamDeclaration                          { [$1] }
                        | ListaParam ',' ParamDeclaration           { $3 : $1 }
                        | {- empty -}                               { [] }

ParamDeclaration        :: { AST.VarDeclaration }
ParamDeclaration        : Id ':' ParamRef Type                      {%do 
                                                                        let varDec = AST.VarDec $1 $4
                                                                        updateOffsetOfEntry $1 $4
                                                                        return varDec }
ParamRef                :: { Bool }
ParamRef                : '>'                                       { True }
                        | {- empty -}                               { False }

Id                      :: { AST.Id }
Id                      : id                                    { AST.Id $1 }
                        -- Boolean literals
                        | maj                                   { AST.Id $1}
                        | min                                   { AST.Id $1 }
                        -- Null expression
                        | TT                                    { AST.Id $1 }

--------------------------------------------------------------------------------
--------------------------For error recovery------------------------------------
--------------------------------------------------------------------------------

MaybeType               :: { Maybe AST.ASTType }
MaybeType               : {- empty -}                           { Nothing }
                        | ':' Type                              { Just $2 }

CloseBracket             : '}'          { True }
                        | error         { False }

ClosePar                : ')'          { True }
                        | error         { False }

CloseAngular            : '>'       { True }
                        | error     { False }

CloseSquare             : ']'       { True }
                        | error     { False }

With                    : with      { True }
                        | error     { False }


In                      : in        { True }
                        | error     { False }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


Block                   :: { AST.Block }
Block                   : PushScope '{' Seq CloseBracket PopScope                  {%do
                                                                                        pushIfError $4 $2 $ matchingError "bracket"
                                                                                        return $ AST.Block $ reverse $ filter notDeclaration $3 }

Seq                     :: { [AST.Instruction] }
Seq                     : Instruction                           { [$1] }
                        | Seq Instruction                       { $2 : $1 }

Instruction             :: { AST.Instruction }
Instruction             : OpenCondition                         { $1 }
                        | ClosedCondition                       { $1 }

OpenCondition           :: { AST.Instruction }
OpenCondition           : if '(' Expression ClosePar Instruction                         {%do
                                                                                            pushIfError $4 $2 $ matchingError "parentheses"
                                                                                            checkExpType $3 [AST.Simple "whole"] $2
                                                                                            return $ AST.IfInst $3 $5 Nothing }
                        | if '(' Expression ClosePar ClosedCondition else OpenCondition  {%do
                                                                                            pushIfError $4 $2 $ matchingError "parentheses"
                                                                                            checkExpType $3 [AST.Simple "whole"] $2
                                                                                            return $ AST.IfInst $3 $5 (Just $7) }

ClosedCondition         :: { AST.Instruction }
ClosedCondition         : if '(' Expression ClosePar ClosedCondition else ClosedCondition    {%do
                                                                                                pushIfError $4 $2 $ matchingError "parentheses"
                                                                                                checkExpType $3 [AST.Simple "whole"] $2
                                                                                                return $ AST.IfInst $3 $5 (Just $7) }
                        | SimpleInstruction                                             { $1 }

SimpleInstruction       :: { AST.Instruction }
SimpleInstruction       : Block                 { AST.BlockInst $1 }
                        | Loop                                  { $1 }
                        | '>>'                                  { AST.NextInst }
                        | '|]'                                  { AST.StopInst }
                        | Return                                { $1 }
                        | Statement '|'                         { $1 }

Statement               :: { AST.Instruction }
Statement               : VarDeclaration                        { AST.VarDecInst $1 }
                        | VarInit                               { $1 }
                        | Asignacion                            { $1 }
                        | IO                                    { $1 }
                        | free Expression                       {% do
                                                                    checkLValueIsAllocated $2 $1
                                                                    return (AST.FreeInst $2) }
                        | LValue '#'                            {% do 
                                                                    checkConstLvalue $1 
                                                                    return $ AST.SharpExp $1 }
                        | LValue '&'                            {% do 
                                                                    checkConstLvalue $1 
                                                                    return $ AST.FlatExp $1 }
                        | CallFuncion                           { AST.CallFuncInst $1 }


VarDeclaration          :: { AST.VarDeclaration }
VarDeclaration          : Id ':' Type                           {% do
                                                                    let varDec = AST.VarDec $1 $3
                                                                    offset <- addOffset $3
                                                                    createVarEntry (AST.id_token $1) (Just $3) (Just offset)
                                                                    return varDec }

VarInit                 :: { AST.Instruction }
VarInit                 : Id ':' Type '<->' Expression          {% do 
                                                                    offset <- addOffset $3
                                                                    maybeEntry <- createVarEntry (AST.id_token $1) (Just $3) (Just offset)

                                                                    idExp <- return $ AST.IdExp $1 $3 maybeEntry
                                                                    let expType = AST.exp_type $5

                                                                    checkAssignment idExp $5 $4
                                                                    return $ AST.AssignInst idExp $ castExp $5 $3 }

Asignacion              :: { AST.Instruction }
Asignacion              : LValue '<->' Expression               {%do
                                                                    checkAssignment $1 $3 $2
                                                                    checkConstLvalue $1

                                                                    let lType = AST.exp_type $1
                                                                        expType = AST.exp_type $3
                                                                    return $ AST.AssignInst $1 $ castExp $3 lType }

LValue                  :: { AST.Expression }
LValue                  : Indexing                              { $1 }
                        | DotExpression                         { $1 }
                        | Dereference                           { $1 }
                        | Id                                    {% do
                                                                    checkVarIsDeclared (AST.id_token $1)
                                                                    idType <- PMonad.lookup $ token $ AST.id_token $1
                                                                    case idType of
                                                                        Nothing ->
                                                                            return AST.errorExp
                                                                        maybeEntry@(Just entry) ->
                                                                            return $ AST.IdExp $1 (fromJust $ AST.entry_type entry) maybeEntry}

Return                  :: { AST.Instruction }
Return                  : Expression '||'                       {% do
                                                                    state <- RWS.get
                                                                    case PMonad.state_ret_type state of
                                                                        Nothing -> do
                                                                            pushError $2 $ show (AST.exp_type $1) ++ " return instruction inside void track:"
                                                                            return $ AST.ReturnInst $ Just $1

                                                                        Just retType -> do
                                                                            checkAssignment' retType $1 $2
                                                                            return $ AST.ReturnInst $ Just $ castExp $1 retType }
                        | '||'                                  {% do
                                                                    state <- RWS.get
                                                                    case PMonad.state_ret_type state of
                                                                        Just retType -> pushError $1 $ "Void return instruction inside " ++ show retType ++ " track:"
                                                                        Nothing -> return ()
                                                                    return $ AST.ReturnInst Nothing}

IO                      :: { AST.Instruction }
IO                      : '@' '(' ListExp ClosePar                   {%do
                                                                        pushIfError $4 $2 $ matchingError "parentheses" 
                                                                        mapM_ (\exp -> checkExpType exp printeableTypes $2) $3
                                                                        return $ AST.RecordInst $ reverse $3 }
                        | '|>' '(' ListExp ClosePar                  {%do
                                                                        pushIfError $4 $2 $ matchingError "parentheses" 
                                                                        mapM_ (\exp -> checkExpType exp printeableTypes $2) $3
                                                                        return $ AST.PlayInst $ reverse $3 }

Loop                    :: { AST.Instruction }
Loop                    : loop PushScope IdConst Block PopScope In '(' Expression ClosePar                                       {%do
                                                                                                                                    -- Error recovery
                                                                                                                                    pushIfError $6 $1 "Missing \"in\" in loop instruction"
                                                                                                                                    pushIfError $9 $7 $ matchingError "parentheses"

                                                                                                                                    checkExpType $8 AST.numberTypes $7
                                                                                                                                    return $ AST.ForInst (fst $3) (fst $ snd $3) (snd $ snd $3) $4 Nothing $8 Nothing }
                        | loop PushScope IdConst Block PopScope In '(' Expression ',' Expression ClosePar                        {%do
                                                                                                                                    -- Error recovery
                                                                                                                                    pushIfError $6 $1 "Missing \"in\" in loop instruction"
                                                                                                                                    pushIfError $11 $7 $ matchingError "parentheses"

                                                                                                                                    checkExpType $8 AST.numberTypes $7
                                                                                                                                    checkExpType $10 AST.numberTypes $9
                                                                                                                                    return $ AST.ForInst (fst $3) (fst $ snd $3) (snd $ snd $3) $4 (Just $8) $10 Nothing }
                        | loop PushScope IdConst Block PopScope In '(' Expression ',' Expression ',' Expression ClosePar         {%do
                                                                                                                                    -- Error recovery
                                                                                                                                    pushIfError $6 $1 "Missing \"in\" in loop instruction"
                                                                                                                                    pushIfError $13 $7 $ matchingError "parentheses"
                                                                                                                                    
                                                                                                                                    checkExpType $8 AST.numberTypes $7
                                                                                                                                    checkExpType $10 AST.numberTypes $9
                                                                                                                                    checkExpType $12 AST.numberTypes $11
                                                                                                                                    return $ AST.ForInst (fst $3) (fst $ snd $3) (snd $ snd $3) $4 (Just $8) $10 (Just $12) }
                        | loop '(' Expression ClosePar Block     {% do
                                                                    pushIfError $4 $2 $ matchingError "parentheses"
                                                                    checkExpType $3 [AST.Simple "whole"] $2
                                                                    return $ AST.WhileInst $3 $5 }
IdConst                 :: { (AST.Id, (Maybe AST.ASTType, AST.Entry)) }
IdConst                 : id MaybeType                                   {% do 
                                                                            case $2 of 
                                                                                Nothing -> do 
                                                                                    offset <- addOffset $ AST.Simple "eighth"
                                                                                    createConstEntry $1 $2 (Just offset)
                                                                                Just typex -> do 
                                                                                    offset <- addOffset typex
                                                                                    createConstEntry $1 $2 (Just offset)
                                                                            Just entry <- PMonad.lookup (token $1) 
                                                                            return $ (AST.Id $1, ($2, entry)) }


CallFuncion             :: { AST.Expression }
CallFuncion             : play Id With '(' ListExp ClosePar          {% do
                                                                        -- Error recovery
                                                                        pushIfError $3 $1 "Missing \"with\" in play instruction:"
                                                                        pushIfError $6 $4 $ matchingError "parentheses"

                                                                        maybeEntry <- checkVarIsDeclared (AST.id_token $2)
                                                                        case maybeEntry of
                                                                            Nothing -> return AST.errorExp
                                                                            Just entry -> do
                                                                                let category = AST.entry_category entry
                                                                                case category of
                                                                                    AST.Function{AST.function_params=params} -> do
                                                                                        -- Verificacion of the list of expressions
                                                                                        casted_args' <- checkParams $4 (reverse $5) params
                                                                                        let casted_args = reverse casted_args'
                                                                                        return $ AST.CallExp $2 casted_args (fromMaybe voidType $ AST.entry_type entry) maybeEntry

                                                                                    _ -> do
                                                                                        pushError $1 "Calling a not track expression:"
                                                                                        return $ AST.CallExp $2 (reverse $5) (fromMaybe voidType $ AST.entry_type entry) maybeEntry }

                        | play Id                               {% do
                                                                            maybeEntry <- checkVarIsDeclared (AST.id_token $2)
                                                                            case maybeEntry of
                                                                                Nothing -> return AST.errorExp
                                                                                Just entry -> do
                                                                                    let category = AST.entry_category entry
                                                                                    case category of
                                                                                        AST.Function{AST.function_params=params} ->
                                                                                            if length params == 0
                                                                                                then return () 
                                                                                                else pushError (AST.id_token $2) "Wrong number of arguments:"
                                                                                        _ -> pushError $1 "Calling a not track expression:"
                                                                                    return $ AST.CallExp $2 [] (fromMaybe voidType $ AST.entry_type entry) maybeEntry }

ListExp                 :: { [AST.Expression] }
ListExp                 : Expression                            { [$1] }
                        | ListExp ',' Expression                { $3 : $1 }
                        | {-empty-}                             { [] }

Indexing                :: { AST.Expression }
Indexing                : Expression '[' Expression CloseSquare     {% do 
                                                                        -- Error recovery
                                                                        pushIfError $4 $2 $ matchingError "square bracket"

                                                                        let expType = AST.exp_type $1
                                                                        -- Check if an error occurred
                                                                        case expType of
                                                                            AST.Simple "Error" -> return AST.errorExp
                                                                            _ -> do
                                                                                -- Check left expression is a melody
                                                                                if AST.type_str expType /= "Melody"
                                                                                    then pushError $2 "Indexing a not melody expression:"
                                                                                    else return ()

                                                                                -- Check index expression is an integer
                                                                                checkExpType $3 [AST.Simple "quarter", AST.Simple "eighth"] $2

                                                                                return $ AST.IndexingExp $1 $3 $2 (AST.type_type $ AST.exp_type $1) }

DotExpression           :: { AST.Expression }
DotExpression           : Expression '.' Id                         {% do
                                                                        let lType = AST.exp_type $1
                                                                        -- Check if an error occurred
                                                                        case lType of
                                                                            AST.Simple "Error" -> do
                                                                                return AST.errorExp
                                                                            _ -> do
                                                                                entryMaybe <- PMonad.lookup $ AST.type_str lType
                                                                                let entry = fromJust entryMaybe
                                                                                    levelMaybe = AST.entry_level entry
                                                                                throwIfNothing levelMaybe $2 "Expression is not a chord or legato:"

                                                                                fieldEntry <- analyzeField (AST.id_token $3) (fromJust levelMaybe)
                                                                                return $ AST.DotExp $1 $3 (fromJust $ AST.entry_type fieldEntry) }

Dereference             :: { AST.Expression }
Dereference             : Expression '!'                            {% do 
                                                                        let expType = AST.exp_type $1
                                                                        case expType of
                                                                            AST.Simple "Error" -> return AST.errorExp
                                                                            _ -> do
                                                                                if AST.type_str expType /= "Sample"
                                                                                    then pushError $2 "Dereferencing a not sample expression:"
                                                                                    else return ()
                                                                                return $ AST.DereferenceExp $1 $2 (AST.type_type $ AST.exp_type $1) }

Type                    :: { AST.ASTType }
Type                    : PrimitiveType                         { $1 }
                        | MelodyType                            { $1 }
                        | sample '<' Type CloseAngular          {%do
                                                                    pushIfError $4 $2 $ matchingError "angle bracket"
                                                                    return $ AST.Compound (token $1) $3 }
                        | IdType                                { $1 }

PrimitiveType           :: { AST.ASTType }
PrimitiveType           : whole                                 { AST.Simple (token $1) }
                        | half                                  { AST.Simple (token $1) }
                        | quarter                               { AST.Simple (token $1) }
                        | eighth                                { AST.Simple (token $1) }
                        | ThirtySecond                          { AST.Simple (token $1) }
                        | SixtyFourth                           { AST.Simple (token $1) }

MelodyType              :: { AST.ASTType }
MelodyType              : melody '<' Type CloseAngular          {%do
                                                                    pushIfError $4 $2 $ matchingError "angle bracket"
                                                                    return $ AST.Compound (token $1) $3 }

Literal                 :: { AST.Expression }
Literal                 : int                                   {%do return $ AST.LiteralExp $1 (AST.Simple "quarter") }
                        | float                                 {%do return $ AST.LiteralExp $1 (AST.Simple "32th") }
                        | string                                {%do
                                                                    return $ AST.LiteralExp $1 (AST.Compound "Melody" $ AST.Simple "half") }
                        | char                                  {%do return $ AST.LiteralExp $1 (AST.Simple "half") }
                        | MelodyLiteral                         { $1 }

                        | IdType '(' ListExp ClosePar           {% do 
                                                                    if $1 == AST.errorType
                                                                        then do
                                                                            return AST.errorExp
                                                                        else do
                                                                            pushIfError $4 $2 $ matchingError "parentheses"

                                                                            -- Check if listExp is equal type of fields
                                                                            Just a <- PMonad.lookup (AST.type_str $1)
                                                                            let fields = fromJust $ AST.type_fields $ AST.entry_category a
                                                                            case (AST.type_adt $ AST.entry_category a) of
                                                                                Just AST.Chord -> do
                                                                                    casted_args <- checkParams $2 (reverse $3) fields
                                                                                    return $ AST.ChordLiteral casted_args $1

                                                                                Just AST.Legato -> do
                                                                                    pushError $2 "This sintax is for chords only:"
                                                                                    return $ AST.LegatoLiteral (AST.Id $2) (head $3) $1} -- Just for returning a legato and error recovering

                        | IdType '(' Id ':' Expression ClosePar     {% do 
                                                                        if $1 == AST.errorType
                                                                            then return AST.errorExp
                                                                            else do
                                                                                pushIfError $6 $2 $ matchingError "parentheses"

                                                                                -- Check if listExp is equal type of fields
                                                                                Just entry <- PMonad.lookup (AST.type_str $1)
                                                                                let fields = fromJust $ AST.type_fields $ AST.entry_category entry
                                                                                case (AST.type_adt $ AST.entry_category entry) of
                                                                                    Just AST.Chord -> do
                                                                                        pushError $2 "This sintax is for legatos only:"
                                                                                        return $ AST.ChordLiteral [$5] $1 -- just for returning a chord

                                                                                    Just AST.Legato -> do
                                                                                        -- Check if `Id` is a field in the legato.
                                                                                        let idToken = token $ AST.id_token $3
                                                                                            equalId = \(AST.VarDec var_id _) -> token (AST.id_token var_id) == idToken
                                                                                        case filter equalId fields of
                                                                                            [] -> do
                                                                                                pushError $2 $ show idToken ++ " is not a field in " ++ show $1 ++ ":"
                                                                                                return $ AST.LegatoLiteral $3 $5 $1 -- just for returning a legato
                                                                                            matchedField -> do
                                                                                                casted_arg <- checkParams $2 [$5] matchedField
                                                                                                return $ AST.LegatoLiteral $3 (head casted_arg) $1}

                        | PrimitiveType '(' Expression ClosePar     {%do
                                                                        pushIfError $4 $2 $ matchingError "parentheses"
                                                                        -- Return a casting expression (if needed)
                                                                        return $ castExp $3 $1}

                        | MelodyType '(' Expression ClosePar        {%do
                                                                        pushIfError $4 $2 $ matchingError "parentheses"
                                                                        checkExpType $3 [AST.Simple "quarter", AST.Simple "eighth"] $2
                                                                        return $ AST.MelodyLiteral' $3 $1 }

MelodyLiteral           :: { AST.Expression }
MelodyLiteral           : '[' ListExp CloseSquare               {%do
                                                                    -- Error recovery
                                                                    pushIfError $3 $1 $ matchingError "square bracket"

                                                                    case $2 of
                                                                        [] -> return $ AST.MelodyLiteral [] (AST.Simple "empty_list")
                                                                        (e:es) -> do
                                                                            let expType = AST.exp_type e
                                                                            if all (equalType expType) $ map AST.exp_type $2
                                                                                then                                                                           
                                                                                    return ()
                                                                                else pushError $1 "Not homogeneous melodies are not allowed:" 

                                                                            return $ AST.MelodyLiteral (reverse $2) (AST.Compound "Melody" expType) }

Expression              :: { AST.Expression }
Expression              : LValue %prec LVALUE                   { $1 }
                        -- Boolean
                        | not Expression                        {%do
                                                                    let expected = AST.Simple "whole"
                                                                    checkExpType $2 [expected] $1
                                                                    return $ AST.NotExp $2 expected }
                        | Expression and Expression             {%do
                                                                    let expected = AST.Simple "whole"
                                                                    checkExpType $1 [expected] $2
                                                                    checkExpType $3 [expected] $2
                                                                    return $ AST.AndExp $1 $3 expected } 
                        | Expression or Expression              {%do
                                                                    let expected = AST.Simple "whole"
                                                                    checkExpType $1 [expected] $2
                                                                    checkExpType $3 [expected] $2
                                                                    return $ AST.OrExp $1 $3 expected } 

                        -- Aritmetivos
                        | '-' Expression %prec NEG              {%do
                                                                    checkExpType $2 AST.numberTypes $1
                                                                    return $ AST.NegativeExp $2 (AST.exp_type $2) }

                        | Expression '-' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.SubstractionExp exp1 exp2 finalType }
                        | Expression mod Expression             {%do
                                                                    checkExpType $1 [AST.Simple "quarter", AST.Simple "eighth"] $2
                                                                    checkExpType $3 [AST.Simple "quarter", AST.Simple "eighth"] $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.ModExp exp1 exp2 finalType }
                        | Expression '/' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.DivExp exp1 exp2 finalType }
                        | Expression '*' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.MultExp exp1 exp2 finalType }
                        | Expression '^' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 [AST.Simple "quarter", AST.Simple "eighth"] $2
                                                                    
                                                                    return $ AST.PowExp $1 $3 (AST.exp_type $1) }             
                        | Expression '+' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.AdditionExp exp1 exp2 finalType }

                        -- Relacionales
                        | Expression '=' Expression             {%do
                                                                    let expected = [AST.exp_type $1]
                                                                    checkEquality $1 $3 $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.EqualExp exp1 exp2 (AST.Simple "whole") }

                        | Expression '/=' Expression            {%do
                                                                    let expected = [AST.exp_type $1]
                                                                    checkEquality $1 $3 $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.NotEqualExp exp1 exp2 (AST.Simple "whole") }
                        | Expression '<' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.LessExp exp1 exp2 (AST.Simple "whole") }
                        | Expression '>' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.GreaterExp exp1 exp2 (AST.Simple "whole") }
                        | Expression '<=' Expression            {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.LessEqualExp exp1 exp2 (AST.Simple "whole") }
                        | Expression '>=' Expression            {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2
                                                                    return $ AST.GreaterEqualExp $1 $3 (AST.Simple "whole") }

                        -- Micelaneos
                        | Literal                               { $1 }
                        | '(' Expression ClosePar               {% do
                                                                    pushIfError $3 $1 $ matchingError "parentheses"
                                                                    return $2 }

                        | new Literal                           {%do 
                                                                    return $ AST.NewExp (Just $2) (AST.Compound "Sample" (AST.exp_type $2)) }

                        | new IdType                            {%do
                                                                    return $ AST.NewExp Nothing (AST.Compound "Sample" $2) }

                        | length '(' Expression ClosePar        {%do
                                                                    pushIfError $4 $2 $ matchingError "parentheses"
                                                                    case AST.exp_type $3 of
                                                                        AST.Simple "Error" -> return AST.errorExp
                                                                        AST.Compound "Melody" _ ->
                                                                            return $ AST.LengthExp $3 $ AST.Simple "quarter"
                                                                        _ -> do
                                                                            pushError $2 "Expression is not a melody:"
                                                                            return AST.errorExp}

                        | CallFuncion                           { $1 }

IdType                  :: { AST.ASTType }
IdType                  : id_type                               {%do
                                                                    entryMaybe <- PMonad.lookup (token $1)
                                                                    case entryMaybe of
                                                                        Nothing -> do
                                                                            pushError $1 "Type not declared in scope:"
                                                                            return AST.errorType
                                                                        Just _ -> return $ AST.Simple (token $1) }

NewType                 :: { String }
NewType                 : chord IdType                         {%do
                                                                    let typeStr = AST.type_str $2
                                                                    computeTypeSize $2
                                                                    return typeStr }
                        | legato IdType                        {%do
                                                                    let typeStr = AST.type_str $2
                                                                    computeTypeSize $2
                                                                    return typeStr }

ChordLegato             :: { () }
ChordLegato             : NewType PushScope ChordLegatoFields PopScope  {% PMonad.updateEntry (addFields $3) $1 }

ChordLegatoFields       : '{' ListaField CloseBracket                    {% do
                                                                            pushIfError $3 $1 $ matchingError "bracket"
                                                                            return $ reverse $2 }

ListaField              :: { [AST.VarDeclaration] }
ListaField              : FieldDeclaration                      { [$1] }
                        | ListaField ',' FieldDeclaration       { $3 : $1 }

FieldDeclaration        :: { AST.VarDeclaration }
FieldDeclaration        : Id ':' Type                           {% do
                                                                    offset <- addOffset $3
                                                                    createFieldEntry (AST.id_token $1) $3 (Just offset)
                                                                    return $ AST.VarDec $1 $3 }

PushScope               :: { () }
PushScope               : {- empty -}                           {% PMonad.pushScope }

PopScope                :: { () }
PopScope                : {- empty -}                           {% PMonad.popScope }

{
printeableTypes = [AST.Simple "quarter", AST.Simple "eighth", AST.Simple "whole", AST.Simple "half", AST.Simple "32th", AST.Simple "64th", AST.Compound "Melody" (AST.Simple "half")]
voidType = AST.Simple "void"
-----------------------------------------------------------------------------------------------
-- Exceptions
-----------------------------------------------------------------------------------------------

-- | Throws a syntatic error
parseError :: [Token] -> PMonad.ParserMonad a
parseError [] = error $ "Source file is not syntatically written well."
parseError (tk:_) = do
    srcFile <- RWS.ask
    throwCompilerError srcFile [Error (line tk) (col tk) "Parse error:"]

-----------------------------------------------------------------------------------------------
-- Entry Creation
-----------------------------------------------------------------------------------------------

createFunctionEntry :: Token -> Maybe AST.ASTType -> AST.Id -> [AST.VarDeclaration] -> Maybe AST.Block -> Maybe Int -> ParserMonad ()
createFunctionEntry tk semType funcId params block offset = do
    a <- verifyFunctionEntry (token tk) 
    if a then do 
        let funcat = AST.Function { AST.function_block = block, AST.function_params = params, AST.max_offset = offset }
        PMonad.ParserState{PMonad.state_scopes=AST.Scopes _ (_:prev:_)} <- RWS.get
        currScope <- PMonad.currScope
        let entry = AST.Entry {
            AST.entry_name = token tk,
            AST.entry_category = funcat,
            AST.entry_scope = prev,
            AST.entry_type = semType,
            AST.entry_level = Just currScope
        }
        PMonad.insertEntry entry
    else
        pushError tk "Function already declared:"

createVarEntry :: Token -> Maybe AST.ASTType -> Maybe Int -> ParserMonad (Maybe AST.Entry)
createVarEntry tk semType maybeOffset = do
    curr <- PMonad.currScope
    result <- verifyVarEntry (token tk) curr
    if result then do 
        let entry = AST.Entry {
            AST.entry_name = token tk,
            AST.entry_category = AST.Var maybeOffset,
            AST.entry_scope = curr,
            AST.entry_type = semType,
            AST.entry_level = Nothing
        }
        PMonad.insertEntry entry
        return $ Just entry
    else do
        pushError tk "Variable already declared in the same scope:"
        return Nothing

createConstEntry :: Token -> Maybe AST.ASTType -> Maybe Int -> ParserMonad ()
createConstEntry tk semType maybeOffset = do 
    curr <- PMonad.currScope
    result <- verifyVarEntry (token tk) curr
    let typeconst = fromMaybe (AST.Simple "quarter") semType  
    if result then do 
        let entry = AST.Entry {
            AST.entry_name = token tk,
            AST.entry_category = AST.Const maybeOffset,
            AST.entry_scope = curr,
            AST.entry_type = Just typeconst,
            AST.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        pushError tk "Const already declared in the same scope:"

createTypeEntry :: Token -> String -> ParserMonad ()
createTypeEntry tk typeStr = do
    result <- verifyTypeEntry typeStr
    if result then do
        st@PMonad.ParserState{PMonad.state_lvl=lvl} <- RWS.get
        curr <- PMonad.currScope

        let entry = AST.Entry {
            AST.entry_name = typeStr,
            AST.entry_category = (AST.Type Nothing (Just $ getAdt tk) (-1)),
            AST.entry_scope = curr,
            AST.entry_type = Nothing,
            AST.entry_level = Just (lvl+1)
        }
        PMonad.insertEntry entry
    else
        pushError tk "Type already declared in same scope:"
    where
        getAdt tk 
            | token tk == "chord"= AST.Chord
            | token tk == "legato"= AST.Legato

computeTypeSize :: AST.ASTType -> ParserMonad Int
computeTypeSize (AST.Simple typeStr) = do
    Just (entry@AST.Entry{AST.entry_category=cat}) <- PMonad.lookup typeStr
    case AST.type_size cat of
        -1 -> do
            -- Traverse types of fields and compute correspondant sizes.            
            let fieldTypes = map AST.var_type $ fromJust $ AST.type_fields cat
            size <- foldM (\sz t -> computeTypeSize t >>= (\w -> return $ sz+w)) 0 fieldTypes

            -- Update entry with computed size
            PMonad.updateEntry (\( e@AST.Entry{ AST.entry_category=c } : _) -> Just $ [ e{ AST.entry_category = c{ AST.type_size=size } } ]) typeStr
            return size

        size -> return size

computeTypeSize (AST.Compound typeStr astType) = return 4


createFieldEntry :: Token -> AST.ASTType -> Maybe Int -> ParserMonad ()
createFieldEntry tk semType maybeOffset = do
    curr <- PMonad.currScope
    result <- verifyFieldEntry (token tk) curr
    entryMaybe <- PMonad.lookup (AST.type_str semType)
    if result then do
        case entryMaybe of
            Nothing -> pushError tk $ "Type " ++ show semType ++ " not declared in scope:"
            Just _ -> do
                let entry = AST.Entry {
                    AST.entry_name = token tk,
                    AST.entry_category = AST.Field maybeOffset,
                    AST.entry_scope = curr,
                    AST.entry_type = Just semType,
                    AST.entry_level = Nothing
                }
                PMonad.insertEntry entry
    else
        pushError tk "Field already declared in same scope:"

createParamEntry :: Token -> Maybe AST.ASTType -> Bool -> ParserMonad ()
createParamEntry tk semType ref = do
    curr <- PMonad.currScope
    result <- verifyParamsEntry (token tk) curr
    if result then do
        let entry = AST.Entry {
            AST.entry_name = token tk,
            AST.entry_category = AST.Param ref Nothing,
            AST.entry_scope = curr,
            AST.entry_type = semType,
            AST.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        pushError tk "Parameter already defined in same track:"
    


        
-----------------------------------------------------------------------------------------------
-- Verification redeclarations
-----------------------------------------------------------------------------------------------
verifyFunctionEntry :: String -> ParserMonad (Bool)
verifyFunctionEntry name = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (AST.Entry _ (AST.Function{}) _ _ _) -> return False
        Just _ -> return True

verifyVarEntry :: String -> Int -> ParserMonad (Bool)
verifyVarEntry name current_scope = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (AST.Entry _ AST.Var{} scope _ _) -> return $ scope /= current_scope
        Just (AST.Entry _ (AST.Const _) scope _ _) -> return $ scope /= current_scope
        Just _ -> return True

verifyTypeEntry :: String -> ParserMonad (Bool)
verifyTypeEntry name = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (AST.Entry _ AST.Type{} _ _ _) -> return False
        Just _ -> return True

verifyFieldEntry :: String -> Int -> ParserMonad (Bool)
verifyFieldEntry name current_scope = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (AST.Entry _ AST.Field{} scope _ _) -> return $ scope /= current_scope
        Just _ -> return True

verifyParamsEntry :: String -> Int -> ParserMonad (Bool)
verifyParamsEntry name current_scope = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (AST.Entry _ AST.Param{} scope _ _) -> return $ scope /= current_scope
        Just _ -> return True

-- | Add block to a function
addBlock :: AST.Block -> Int -> [AST.Entry] -> Maybe [AST.Entry]
addBlock block offset lst = Just $ map mapping lst
    where 
        condition e = 
            case AST.entry_category e of
                AST.Function{AST.function_block=Nothing} -> True
                _ -> False
        mapping e = if condition e then e { AST.entry_category = (AST.entry_category e) { AST.function_block = Just block, AST.max_offset = Just offset } }
                else e

addFields :: [AST.VarDeclaration] -> [AST.Entry] -> Maybe [AST.Entry]
addFields listFields lst = Just $ map mapping lst
    where 
        condition entry = 
            case AST.entry_category entry of
                AST.Type{AST.type_fields=Nothing}-> True
                _ -> False
        mapping entry = if condition entry then entry { AST.entry_category = (AST.entry_category entry) { AST.type_fields = Just listFields } }
                else entry
--------------------------------------------
----------- Offset Operations --------------
--------------------------------------------
addOffset :: AST.ASTType -> ParserMonad Int
addOffset varType = do
    size <- computeTypeSize varType
    PMonad.getAndIncOffset size

updateOffsetOfEntry varId varType = do
    offset <- addOffset varType
    scope <- PMonad.currScope
    PMonad.updateEntry (\lst -> Just $ map (updateVarDec varId varType scope offset) lst) (token $ AST.id_token varId)


updateVarDec :: AST.Id -> AST.ASTType -> Int -> Int -> AST.Entry -> AST.Entry
updateVarDec AST.Id{AST.id_token=idToken} varType scope offset entry@AST.Entry{AST.entry_name=entryName, AST.entry_scope=entryScope, AST.entry_type=Just entryType} =
    if token idToken == entryName && varType == entryType && scope == entryScope
        then case AST.entry_category entry of
            AST.Var Nothing -> entry{AST.entry_category=AST.Var $ Just offset}
            AST.Field Nothing -> entry{AST.entry_category=AST.Field $ Just offset}
            cat@AST.Param{AST.offset=Nothing} -> entry{ AST.entry_category = cat{ AST.offset = Just offset } }
        else entry

updateVarDec _ _ _ _ e = e

--------------------------------------------
----------------- END ----------------------
--------------------------------------------
}