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
    eight              { EightToken _ _ _ }
    melody              { MelodyToken _ _ _ }
    sample              { SampleToken _ _ _ }
    ThirtySecond        { ThirtySecondToken _ _ _ }
    SixtyFourth         { SixtyFourthToken _ _ _ }

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
%nonassoc '>' '<' '=' '/=' '<=' '>='
%left LVALUE
%right '['
%left ']'
%left '+' '-'
%left '*' '/' mod
%left '^'
%left NEG '#' '&'
%left and or
%right not
%left '.'
%left '!'

%%

Start                   :: { () }
Start                   : ExternalList                      {  }

ExternalList            :: { () }
ExternalList            : ExternalList FunctionDeclaration  { }             
                        | ExternalList ChordLegato          { }
                        | {- empty -}                       { }

-- TODO: Agregar los params y los fields en un nuevo scope
FunctionDeclaration     :: { () }                                           -- add block to function entry
FunctionDeclaration     : Signature Block                                   {% do
                                                                                let addBlock [e] = Just [e { SemData.entry_category = (SemData.entry_category e) { SemData.function_block = Just $2 } }]
                                                                                PMonad.updateEntry addBlock $1 }

                        | main PushScope '(' ')' Block                      {% let idMain = AST.Id $1 in createFunctionEntry (AST.id_token idMain) Nothing idMain [] (Just $5) }

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
ParamDeclaration        : Id ':' Type                           {% do
                                                                    createParamEntry (AST.id_token $1) (Just $3)
                                                                    return $ AST.VarDec $1 $3 }

Id                      :: { AST.Id }
Id                      : id                                    { AST.Id $1 }

MaybeType               :: { Maybe AST.Type }
MaybeType               : {- empty -}                           { Nothing }
                        | ':' Type                              { Just $2 }

Block                   :: { AST.Block }
Block                   : '{' Seq '}' PopScope                  { AST.Block $ reverse $2 }

Seq                     :: { [AST.Instruction] }
Seq                     : Instruction                           { [$1] }
                        | Seq Instruction                       { $2 : $1 }

Instruction             :: { AST.Instruction }
Instruction             : OpenCondition                         { $1 }
                        | ClosedCondition                       { $1 }

OpenCondition           :: { AST.Instruction }
OpenCondition           : if '(' Expression ')' Instruction                         { AST.IfInst $3 $5 Nothing }
                        | if '(' Expression ')' ClosedCondition else OpenCondition  { AST.IfInst $3 $5 (Just $7) }

ClosedCondition         :: { AST.Instruction }
ClosedCondition         : if '(' Expression ')' ClosedCondition else ClosedCondition    { AST.IfInst $3 $5 (Just $7) }
                        | SimpleInstruction                                             { $1 }

SimpleInstruction       :: { AST.Instruction }
SimpleInstruction       : PushScope Block                  { AST.BlockInst $2 }
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
                                                                    analyzeVar (AST.id_token $2)
                                                                    return (AST.FreeInst $2) }
                        | LValue '#'                            { AST.SharpExp $1 }
                        | LValue '&'                            { AST.FlatExp $1 }


VarDeclaration          :: { AST.VarDeclaration }
VarDeclaration          : Id ':' Type                           {% do
                                                                    createVarEntry (AST.id_token $1) (Just $3)
                                                                    return $ AST.VarDec $1 $3}

VarInit                 :: { AST.Instruction }
VarInit                 : Id ':' Type '<->' Expression          {% do 
                                                                    createVarEntry (AST.id_token $1) (Just $3)
                                                                    return $ AST.AssignInst (AST.IdExp $1) $5 }

Asignacion              :: { AST.Instruction }
Asignacion              : LValue '<->' Expression               { AST.AssignInst $1 $3 }

LValue                  :: { AST.Expression }
LValue                  : Indexing                              { $1 }
                        | DotExpression                         { $1 }
                        | Dereference                           { $1 }
                        | Id                                    {% analyzeVar (AST.id_token $1) >> return (AST.IdExp $1) }

Return                  :: { AST.Instruction }
Return                  : Expression '||'                       { AST.ReturnInst $1 }

IO                      :: { AST.Instruction }
IO                      : '@' '(' ListExp ')'                   { AST.RecordInst $ reverse $3 }
                        | '|>' '(' ListExp ')'                  { AST.PlayInst $ reverse $3 }

Loop                    :: { AST.Instruction }
Loop                    : loop PushScope Id MaybeType Block in '(' Expression ')'                                       {AST.ForInst $3 $4 $5 Nothing $8 Nothing }
                        | loop PushScope Id MaybeType Block in '(' Expression ',' Expression ')'                        {AST.ForInst $3 $4 $5 (Just $8) $10 Nothing }
                        | loop PushScope Id MaybeType Block in '(' Expression ',' Expression ',' Expression ')'         {AST.ForInst $3 $4 $5 (Just $8) $10 (Just $12) }
                        | loop '(' Expression ')' Block                                                                      {AST.WhileInst $3 $5 }

CallFuncion             :: { AST.Expression }
CallFuncion             : play Id with '(' ListExp ')'          {% do
                                                                    analyzeVar (AST.id_token $2)
                                                                    return (AST.CallExp $2 $ reverse $5) }

ListExp                 :: { [AST.Expression] }
ListExp                 : Expression                            { [$1] }
                        | ListExp ',' Expression                { $3 : $1 }

Indexing                :: { AST.Expression }
Indexing                : Expression '[' Expression ']'             { AST.IndexingExp $1 $3 $2 }

DotExpression           :: { AST.Expression }
DotExpression           : Expression '.' Id                         {% let ast = AST.DotExp $1 $3 in analyzeExpression ast >> return ast }

Dereference             :: { AST.Expression }
Dereference             : Expression '!'                            { AST.DereferenceExp $1 $2 }

Type                    :: { AST.Type }
Type                    : whole                                 { AST.Type $1 Nothing }
                        | half                                  { AST.Type $1 Nothing }
                        | quarter                               { AST.Type $1 Nothing }
                        | eight                                 { AST.Type $1 Nothing }
                        | ThirtySecond                          { AST.Type $1 Nothing }
                        | SixtyFourth                           { AST.Type $1 Nothing }
                        | melody '<' Type '>'                   { AST.Type $1 (Just $3) }
                        | sample '<' Type '>'                   { AST.Type $1 (Just $3) }
                        | IdType                                { $1 }

Literal                 :: { AST.Expression }
Literal                 : int                                   { AST.Literal $1 }
                        | float                                 { AST.Literal $1 }
                        | maj                                   { AST.Literal $1 }
                        | min                                   { AST.Literal $1 }
                        | string                                { AST.Literal $1 }
                        | char                                  { AST.Literal $1 }
                        | LiteralMelody                         { $1 }
                        | Type '(' ListExp ')'                  { AST.Literal' (reverse $3) $1 }

LiteralMelody           :: { AST.Expression }
LiteralMelody           : '[' ListExp ']'                       { AST.LiteralMelody $2 }

Expression              :: { AST.Expression }
Expression              : LValue %prec LVALUE                   { $1 }
                        -- Boolean
                        | not Expression                        { AST.NotExp $2 }
                        | Expression and Expression             { AST.AndExp $1 $3 } 
                        | Expression or Expression              { AST.OrExp $1 $3 }

                        -- Aritmetivos
                        | '-' Expression %prec NEG              { AST.NegativeExp $2 }
                        | Expression '-' Expression             { AST.SubstractionExp $1 $3 }
                        | Expression mod Expression             { AST.ModExp $1 $3 }
                        | Expression '/' Expression             { AST.DivExp $1 $3 }
                        | Expression '*' Expression             { AST.MultExp $1 $3 }
                        | Expression '^' Expression             { AST.PowExp $1 $3 }             
                        | Expression '+' Expression             { AST.AdditionExp $1 $3 }

                        -- Relacionales
                        | Expression '=' Expression             { AST.EqualExp $1 $3 }
                        | Expression '/=' Expression            { AST.NotEqualExp $1 $3 }
                        | Expression '<' Expression             { AST.LessExp $1 $3 }
                        | Expression '>' Expression             { AST.GreaterExp $1 $3 }
                        | Expression '<=' Expression            { AST.LessEqualExp $1 $3 }
                        | Expression '>=' Expression            { AST.GreaterEqualExp $1 $3 }

                        -- Micelaneos
                        | Literal                               { $1 }
                        | '(' Expression ')'                    { $2 }

                        | new Literal                           { AST.NewExp $2 }

                        | CallFuncion                           { $1 }

IdType                  :: { AST.Type }
IdType                  : id_type                               { AST.Type $1 Nothing }

NewType                 :: { () }
NewType                 : chord IdType                         {% createTypeEntry $ AST.type_token $2 }
                        | legato IdType                        {% createTypeEntry $ AST.type_token $2 }

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
parseError (tk:_) = do
    srcFile <- RWS.ask
    throwCompilerError srcFile [Error (line tk) (col tk) "Parse error:"]

-----------------------------------------------------------------------------------------------
-- Entry Creation
-----------------------------------------------------------------------------------------------
-- TODO: Recibir el AST como parametro para eficiencia de memoria
createFunctionEntry :: Token -> Maybe AST.Type -> AST.Id -> [AST.VarDeclaration] -> Maybe AST.Block -> ParserMonad ()
createFunctionEntry tk funcType funcId params block = do
    a <- verifyFunctionEntry (token tk) 
    if a then do 
        let funcat = SemData.Function { SemData.function_block = block, SemData.function_params = params }
        semType <- astTypeToSemType funcType
        (SemData.Scopes _ (_:prev:_), _, _) <- RWS.get
        
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = funcat,
            SemData.entry_scope = prev,
            SemData.entry_type = semType,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        semError tk "Error Semantico: Funcion ya declarada"

createVarEntry :: Token -> Maybe AST.Type -> ParserMonad ()
createVarEntry tk t = do
    curr <- PMonad.currScope
    result <- verifyVarEntry (token tk) curr
    if result then do 
        semType <- astTypeToSemType t
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = SemData.Var,
            SemData.entry_scope = curr,
            SemData.entry_type = semType,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        semError tk "Error Semantico: Variable ya declarada en el mismo scope"

createTypeEntry :: Token -> ParserMonad ()
createTypeEntry tk = do
    result <- verifyTypeEntry (token tk)
    if result then do
        st@(_, _, lvl) <- RWS.get
        curr <- PMonad.currScope

        -- liftIO $ putStr "\n" >> print st

        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = SemData.Type,
            SemData.entry_scope = curr,
            SemData.entry_type = Nothing,
            SemData.entry_level = Just (lvl+1)
        }
        PMonad.insertEntry entry
    else
        semError tk "Error Semantico: Campo ya declarado en el mismo scope"
    -- st' <- RWS.get
    -- liftIO $ putStr "\n" >> print st'

createFieldEntry :: Token -> Maybe AST.Type -> ParserMonad ()
createFieldEntry tk typ = do
    curr <- PMonad.currScope
    result <- verifyFieldEntry (token tk) curr
    if result then do
        semType <- astTypeToSemType typ
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = SemData.Field,
            SemData.entry_scope = curr,
            SemData.entry_type = semType,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        semError tk "Error Semantico: Campo ya declarado en el mismo scope"

createParamEntry :: Token -> Maybe AST.Type -> ParserMonad ()
createParamEntry tk typ = do
    curr <- PMonad.currScope
    result <- verifyParamsEntry (token tk) curr
    if result then do
        semType <- astTypeToSemType typ
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = SemData.Param,
            SemData.entry_scope = curr,
            SemData.entry_type = semType,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        semError tk "Error Semantico: Campo ya declarado en el mismo scope"
    


        
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
        Just (SemData.Entry _ SemData.Param scope _ _) -> return $ scope /= current_scope
        Just _ -> return True



-- addBlock :: [a] -> Maybe [b]
-- addBlock [] = Nothing

-- addBlock items =

--------------------------------------------
----------------- END ----------------------
--------------------------------------------
}