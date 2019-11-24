{
module Parser.Parser(parse, PMonad.ParserMonad) where
import Lexer
import Tokens
import qualified AST
import Util.Error
import Data.Either
import Data.Maybe
import Parser.Monad (ParserMonad)
import qualified Parser.Monad as PMonad
import qualified Control.Monad.RWS.Lazy as RWS
import Control.Monad.Trans
import qualified Semantic.Data as SemData 
import Semantic.Analyzers
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

    int                 { IntToken _ _ _ }
    float               { FloatToken _ _ _ }
    maj                 { MajToken _ _ _ }
    min                 { MinToken _ _ _ }
    string              { StringToken _ _ _ }
    char                { CharToken _ _ _ }

    id                  { IdToken _ _ _ }
    id_type             { IdTypeToken _ _ _ }

    main                { MainToken _ _ _ }


%nonassoc '<->'
%nonassoc '=' '/=' 
%nonassoc '>' '<' '<=' '>='
%left LVALUE
%left '+' '-'
%left '*' '/' mod
%left '^'
%left NEG '#' '&'
%left and or
%right not
%right '['
%left ']'
%left '!'
%left '.'

%%

Start                   :: { () }
Start                   : ExternalList MainDeclaration      { }

ExternalList            :: { () }
ExternalList            : ExternalList FunctionDeclaration  { }             
                        | ExternalList ChordLegato          { }
                        | {- empty -}                       { }

-- TODO: Agregar los params y los fields en un nuevo scope
FunctionDeclaration     :: { () }                                           -- add block to function entry
FunctionDeclaration     : Signature Block PopScope                          {% PMonad.updateEntry (addBlock $2) $1 }
MainDeclaration         : main PushScope '(' ')' Block                      {% let idMain = AST.Id $1 in createFunctionEntry (AST.id_token idMain) Nothing idMain [] (Just $5) }

Signature               :: { String }
Signature               : track Id PushScope '(' ListaParam ')' MaybeType             {% do
                                                                                            let tk = AST.id_token $2
                                                                                            createFunctionEntry tk $7 $2 (reverse $5) Nothing
                                                                                            return $ token tk }

ListaParam              :: { [AST.VarDeclaration] }
ListaParam              : ParamDeclaration                          { [$1] }
                        | ListaParam ',' ParamDeclaration           { $3 : $1 }
                        | {- empty -}                               { [] }

ParamDeclaration        :: { AST.VarDeclaration }
ParamDeclaration        : Id ':' ParamRef Type                      {% do
                                                                    createParamEntry (AST.id_token $1) (Just $4) $3
                                                                    return $ AST.VarDec $1 $4 }
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

MaybeType               :: { Maybe AST.Type }
MaybeType               : {- empty -}                           { Nothing }
                        | ':' Type                              { Just $2 }

Block                   :: { AST.Block }
Block                   : PushScope '{' Seq '}' PopScope                  { AST.Block $ reverse $3 }

Seq                     :: { [AST.Instruction] }
Seq                     : Instruction                           { [$1] }
                        | Seq Instruction                       { $2 : $1 }

Instruction             :: { AST.Instruction }
Instruction             : OpenCondition                         { $1 }
                        | ClosedCondition                       { $1 }

OpenCondition           :: { AST.Instruction }
OpenCondition           : if '(' Expression ')' Instruction                         {%do
                                                                                        checkExpType $3 [AST.Simple "whole"] $2
                                                                                        return $ AST.IfInst $3 $5 Nothing }
                        | if '(' Expression ')' ClosedCondition else OpenCondition  {%do
                                                                                        checkExpType $3 [AST.Simple "whole"] $2
                                                                                        return $ AST.IfInst $3 $5 (Just $7) }

ClosedCondition         :: { AST.Instruction }
ClosedCondition         : if '(' Expression ')' ClosedCondition else ClosedCondition    {%do
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
                        | free Id                               {% do
                                                                    checkVarIsDeclared (AST.id_token $2)
                                                                    return (AST.FreeInst $2) }
                        | LValue '#'                            { AST.SharpExp $1 }
                        | LValue '&'                            { AST.FlatExp $1 }
                        | CallFuncion                           { AST.CallFuncInst $1 }


VarDeclaration          :: { AST.VarDeclaration }
VarDeclaration          : Id ':' Type                           {% do
                                                                    createVarEntry (AST.id_token $1) (Just $3)
                                                                    return $ AST.VarDec $1 $3}

VarInit                 :: { AST.Instruction }
VarInit                 : Id ':' Type '<->' Expression          {% do 
                                                                    createVarEntry (AST.id_token $1) (Just $3)
                                                                    let idExp = AST.IdExp $1 $3
                                                                    checkEquality idExp $5 $4
                                                                    return $ AST.AssignInst idExp $5 }

Asignacion              :: { AST.Instruction }
Asignacion              : LValue '<->' Expression               {%do
                                                                    checkEquality $1 $3 $2
                                                                    return $ AST.AssignInst $1 $3 }

LValue                  :: { AST.Expression }
LValue                  : Indexing                              { $1 }
                        | DotExpression                         { $1 }
                        | Dereference                           { $1 }
                        | Id                                    {% do
                                                                    checkVarIsDeclared (AST.id_token $1)
                                                                    idType <- PMonad.lookup $ token $ AST.id_token $1
                                                                    return $ (AST.IdExp $1 (fromJust $ SemData.entry_type $ fromJust idType)) }

Return                  :: { AST.Instruction }
Return                  : Expression '||'                       { AST.ReturnInst $ Just $1 }
                        | '||'                                  { AST.ReturnInst Nothing }

IO                      :: { AST.Instruction }
IO                      : '@' '(' ListExp ')'                   { AST.RecordInst $ reverse $3 }
                        | '|>' '(' ListExp ')'                  { AST.PlayInst $ reverse $3 }

Loop                    :: { AST.Instruction }
Loop                    : loop PushScope Id MaybeType Block PopScope in '(' Expression ')'                                       {%do
                                                                                                                                    checkExpType $9 AST.numberTypes $8
                                                                                                                                    return $ AST.ForInst $3 $4 $5 Nothing $9 Nothing }
                        | loop PushScope Id MaybeType Block PopScope in '(' Expression ',' Expression ')'                        {%do
                                                                                                                                    checkExpType $9 AST.numberTypes $8
                                                                                                                                    checkExpType $11 AST.numberTypes $10
                                                                                                                                    return $ AST.ForInst $3 $4 $5 (Just $9) $11 Nothing }
                        | loop PushScope Id MaybeType Block PopScope in '(' Expression ',' Expression ',' Expression ')'         {%do
                                                                                                                                    checkExpType $9 AST.numberTypes $8
                                                                                                                                    checkExpType $11 AST.numberTypes $10
                                                                                                                                    checkExpType $13 AST.numberTypes $12
                                                                                                                                    return $ AST.ForInst $3 $4 $5 (Just $9) $11 (Just $13) }
                        | loop '(' Expression ')' Block                                                                          {% do
                                                                                                                                    checkExpType $3 [AST.Simple "whole"] $2
                                                                                                                                    return $ AST.WhileInst $3 $5 }

CallFuncion             :: { AST.Expression }
CallFuncion             : play Id with '(' ListExp ')'          {% do
                                                                    entry <- checkVarIsDeclared (AST.id_token $2)
                                                                    let category = SemData.entry_category entry

                                                                    case category of
                                                                        SemData.Function _ params ->
                                                                            if length $5 /= length params
                                                                                then semError $4 "Wrong number of arguments:"
                                                                                else return $ AST.CallExp $2 (reverse $5) (fromJust $ SemData.entry_type entry) 
                                                                        _ -> semError $1 "Calling a not track expression:"}

                        | play Id                               {% do
                                                                    entry <- checkVarIsDeclared (AST.id_token $2)
                                                                    let category = SemData.entry_category entry
                                                                    case category of
                                                                        SemData.Function _ params ->
                                                                            if length params == 0
                                                                                then return $ AST.CallExp $2 [] (fromJust $ SemData.entry_type entry) 
                                                                                else semError (AST.id_token $2) "Wrong number of arguments:"
                                                                        _ -> semError $1 "Calling a not track expression:" }

ListExp                 :: { [AST.Expression] }
ListExp                 : Expression                            { [$1] }
                        | ListExp ',' Expression                { $3 : $1 }
                        | {-empty-}                             { [] }

Indexing                :: { AST.Expression }
Indexing                : Expression '[' Expression ']'             {% do 
                                                                        let expType = AST.exp_type $1
                                                                        if AST.type_str expType /= "Melody"
                                                                            then semError $2 "Indexing a not melody expression:"
                                                                            else return $ AST.IndexingExp $1 $3 $2 (AST.type_type $ AST.exp_type $1) }

DotExpression           :: { AST.Expression }
DotExpression           : Expression '.' Id                         {% do
                                                                        let lType = AST.exp_type $1
                                                                        entryMaybe <- PMonad.lookup $ AST.type_str lType
                                                                        let entry = fromJust entryMaybe
                                                                        let levelMaybe = SemData.entry_level entry
                                                                        throwIfNothing levelMaybe $2 "Expression is not a chord or legato:"

                                                                        fieldEntry <- analyzeField (AST.id_token $3) (fromJust levelMaybe)
                                                                        return $ AST.DotExp $1 $3 (fromJust $ SemData.entry_type fieldEntry) }

Dereference             :: { AST.Expression }
Dereference             : Expression '!'                            {% do 
                                                                        let expType = AST.exp_type $1
                                                                        if AST.type_str expType /= "Sample"
                                                                            then semError $2 "Dereferencing a not sample expression:"
                                                                            else return $ AST.DereferenceExp $1 $2 (AST.type_type $ AST.exp_type $1) }

Type                    :: { AST.Type }
Type                    : whole                                 { AST.Simple (token $1) }
                        | half                                  { AST.Simple (token $1) }
                        | quarter                               { AST.Simple (token $1) }
                        | eighth                                { AST.Simple (token $1) }
                        | ThirtySecond                          { AST.Simple (token $1) }
                        | SixtyFourth                           { AST.Simple (token $1) }
                        | melody '<' Type '>'                   { AST.Compound (token $1) $3 }
                        | sample '<' Type '>'                   { AST.Compound (token $1) $3 }
                        | IdType                                { $1 }

Literal                 :: { AST.Expression }
Literal                 : int                                   { AST.Literal $1 (AST.Simple "quarter") }
                        | float                                 { AST.Literal $1 (AST.Simple "eighth") }
                        | string                                { AST.Literal $1 (AST.Compound "Melody" $ AST.Simple "half") }
                        | char                                  { AST.Literal $1 (AST.Simple "half") }
                        | LiteralMelody                         { $1 }
                        | Type '(' ListExp ')'                  { AST.Literal' (reverse $3) $1 }
                        | Type                                  { AST.Literal' [] $1 }

-- TODO: chequear que todos los elementos de la ListExp sean del mismo tipo.
LiteralMelody           :: { AST.Expression }
LiteralMelody           : '[' ListExp ']'                       {%do
                                                                    case $2 of
                                                                        [] -> return $ AST.LiteralMelody [] (AST.Simple "empty_list")
                                                                        (e:es) -> do
                                                                            let expType = AST.exp_type e
                                                                            if all (==expType) $ map AST.exp_type $2
                                                                                then return $ AST.LiteralMelody [] (AST.Compound "Melody" expType)
                                                                                else semError $1 "Not homogeneous melodies are not allowed:" }

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
                                                                    return $ AST.SubstractionExp $1 $3 (max (AST.exp_type $1) (AST.exp_type $3)) }
                        | Expression mod Expression             {%do
                                                                    checkExpType $1 [AST.Simple "quarter", AST.Simple "eighth"] $2
                                                                    checkExpType $3 [AST.Simple "quarter", AST.Simple "eighth"] $2
                                                                    return $ AST.ModExp $1 $3 (max (AST.exp_type $1) (AST.exp_type $3)) }
                        | Expression '/' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2
                                                                    return $ AST.DivExp $1 $3 (max (AST.exp_type $1) (AST.exp_type $3)) }
                        | Expression '*' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2
                                                                    return $ AST.MultExp $1 $3 (max (AST.exp_type $1) (AST.exp_type $3)) }
                        | Expression '^' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 [AST.Simple "quarter", AST.Simple "eighth"] $2
                                                                    return $ AST.PowExp $1 $3 (AST.exp_type $1) }             
                        | Expression '+' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2
                                                                    return $ AST.AdditionExp $1 $3 (max (AST.exp_type $1) (AST.exp_type $3)) }

                        -- Relacionales
                        | Expression '=' Expression             {%do
                                                                    let expected = [AST.exp_type $1]
                                                                    checkEquality $1 $3 $2
                                                                    return $ AST.EqualExp $1 $3 (AST.Simple "whole") }
                        | Expression '/=' Expression            {%do
                                                                    let expected = [AST.exp_type $1]
                                                                    checkEquality $1 $3 $2
                                                                    return $ AST.NotEqualExp $1 $3 (AST.Simple "whole") }
                        | Expression '<' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2
                                                                    return $ AST.LessExp $1 $3 (AST.Simple "whole") }
                        | Expression '>' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2
                                                                    return $ AST.GreaterExp $1 $3 (AST.Simple "whole") }
                        | Expression '<=' Expression            {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2
                                                                    return $ AST.LessEqualExp $1 $3 (AST.Simple "whole") }
                        | Expression '>=' Expression            {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2
                                                                    return $ AST.GreaterEqualExp $1 $3 (AST.Simple "whole") }

                        -- Micelaneos
                        | Literal                               { $1 }
                        | '(' Expression ')'                    { $2 }

                        | new Literal                           { AST.NewExp $2 (AST.Compound "Sample" (AST.exp_type $2)) }

                        | CallFuncion                           { $1 }

IdType                  :: { AST.Type }
IdType                  : id_type                               { AST.Simple (token $1) }

NewType                 :: { () }
NewType                 : chord IdType                         {% createTypeEntry $1 (AST.type_str $2) }
                        | legato IdType                        {% createTypeEntry $1 (AST.type_str $2) }

ChordLegato             :: { () }
ChordLegato             : NewType PushScope ChordLegatoFields PopScope  { }

ChordLegatoFields       : '{' ListaField '}'                    { reverse $2 }

ListaField              :: { [AST.VarDeclaration] }
ListaField              : FieldDeclaration                      { [$1] }
                        | ListaField ',' FieldDeclaration       { $3 : $1 }

FieldDeclaration        :: { AST.VarDeclaration }
FieldDeclaration        : Id ':' Type                           {% do
                                                                    createFieldEntry (AST.id_token $1) (Just $3) 
                                                                    return $ AST.VarDec $1 $3 }

PushScope               :: { () }
PushScope               : {- empty -}                           {% PMonad.pushScope }

PopScope                :: { () }
PopScope                : {- empty -}                           {% PMonad.popScope }

{

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

createFunctionEntry :: Token -> Maybe AST.Type -> AST.Id -> [AST.VarDeclaration] -> Maybe AST.Block -> ParserMonad ()
createFunctionEntry tk semType funcId params block = do
    a <- verifyFunctionEntry (token tk) 
    if a then do 
        let funcat = SemData.Function { SemData.function_block = block, SemData.function_params = params }
        (PMonad.ParserState (SemData.Scopes _ (_:prev:_)) _ _ _) <- RWS.get
        
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = funcat,
            SemData.entry_scope = prev,
            SemData.entry_type = semType,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        semError tk "Function already declared:"

createVarEntry :: Token -> Maybe AST.Type -> ParserMonad ()
createVarEntry tk semType = do
    curr <- PMonad.currScope
    result <- verifyVarEntry (token tk) curr
    if result then do 
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = SemData.Var,
            SemData.entry_scope = curr,
            SemData.entry_type = semType,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        semError tk "Variable already declared in the same scope:"

createTypeEntry :: Token -> String -> ParserMonad ()
createTypeEntry tk typeStr = do
    result <- verifyTypeEntry typeStr
    if result then do
        st@(PMonad.ParserState _ _ lvl _) <- RWS.get
        curr <- PMonad.currScope

        let entry = SemData.Entry {
            SemData.entry_name = typeStr,
            SemData.entry_category = SemData.Type,
            SemData.entry_scope = curr,
            SemData.entry_type = Nothing,
            SemData.entry_level = Just (lvl+1)
        }
        PMonad.insertEntry entry
    else
        semError tk "Type already declared in same scope:"

createFieldEntry :: Token -> Maybe AST.Type -> ParserMonad ()
createFieldEntry tk semType = do
    curr <- PMonad.currScope
    result <- verifyFieldEntry (token tk) curr
    if result then do
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = SemData.Field,
            SemData.entry_scope = curr,
            SemData.entry_type = semType,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        semError tk "Field already declared in same scope:"

createParamEntry :: Token -> Maybe AST.Type -> Bool -> ParserMonad ()
createParamEntry tk semType ref = do
    curr <- PMonad.currScope
    result <- verifyParamsEntry (token tk) curr
    if result then do
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = SemData.Param ref,
            SemData.entry_scope = curr,
            SemData.entry_type = semType,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        semError tk "Parameter already defined in same track:"
    


        
-----------------------------------------------------------------------------------------------
-- Verification redeclarations
-----------------------------------------------------------------------------------------------
verifyFunctionEntry :: String -> ParserMonad (Bool)
verifyFunctionEntry name = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (SemData.Entry _ (SemData.Function _ _) _ _ _) -> return False
        Just _ -> return True

verifyVarEntry :: String -> Int -> ParserMonad (Bool)
verifyVarEntry name current_scope = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (SemData.Entry _ SemData.Var scope _ _) -> return $ scope /= current_scope
        Just _ -> return True

verifyTypeEntry :: String -> ParserMonad (Bool)
verifyTypeEntry name = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (SemData.Entry _ SemData.Type _ _ _) -> return False
        Just _ -> return True

verifyFieldEntry :: String -> Int -> ParserMonad (Bool)
verifyFieldEntry name current_scope = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (SemData.Entry _ SemData.Field scope _ _) -> return $ scope /= current_scope
        Just _ -> return True

verifyParamsEntry :: String -> Int -> ParserMonad (Bool)
verifyParamsEntry name current_scope = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (SemData.Entry _ (SemData.Param _) scope _ _) -> return $ scope /= current_scope
        Just _ -> return True



addBlock :: AST.Block -> [SemData.Entry] -> Maybe [SemData.Entry]
addBlock block lst = Just $ map mapping lst
    where 
        condition e = 
            case SemData.entry_category e of
                SemData.Function Nothing _ -> True
                _ -> False
        mapping e = if condition e then e { SemData.entry_category = (SemData.entry_category e) { SemData.function_block = Just block } }
                else e

--------------------------------------------
-----------Chequear tipos ------------------
--------------------------------------------

checkExpType :: AST.Expression -> [AST.Type] -> Token -> ParserMonad AST.Type
checkExpType exp expected tk = do
    let expType = AST.exp_type exp
    if filter (==expType) expected == []
        then semError tk $ "Expression is not of type " ++ (AST.type_str $ expected!!0) ++ ":"
        else return expType

checkEquality :: AST.Expression -> AST.Expression -> Token -> ParserMonad AST.Type
checkEquality idExp exp tk =
    let astType = AST.exp_type idExp in(
    if AST.type_str astType `elem` ["Sample", "null"]
        then checkExpType exp [astType, AST.Simple "null"] tk
        else if AST.type_str astType `elem` ["Melody", "empty_list"]
            then checkExpType exp [astType, AST.Simple "empty_list"] tk
            else checkExpType exp [astType] tk)
--------------------------------------------
----------------- END ----------------------
--------------------------------------------
}