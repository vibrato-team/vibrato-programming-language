{
module Parser.Parser(parse, PMonad.ParserMonad) where
import Lexer
import Tokens
import qualified AST
import Util.Error
import Data.Either
import qualified Semantic.Data as Sem
import Parser.Monad (ParserMonad)
import qualified Parser.Monad as PMonad
import qualified Control.Monad.RWS.Lazy as RWS
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

Start                   :: { AST.Program }
Start                   : ExternalList                      { AST.Start $ reverse $1 }

ExternalList            :: { [AST.ExternalInstruction] }
ExternalList            : ExternalInstruction                   { [$1] }
                        | ExternalList ExternalInstruction      { $2 : $1 }

ExternalInstruction     :: { AST.ExternalInstruction }
ExternalInstruction     : FunctionDeclaration                   { AST.ExternalFunctionDeclaration $1 }
                        | Instruction                           { AST.ExternalInstruction $1 }

FunctionDeclaration     :: { AST.FunctionDeclaration }
FunctionDeclaration     : track Id '(' ListaParam ')' MaybeType Block     {% createFuntionEntry id_token $2 $6 (reverse $4) $7 } -- AST.FunctionDec
                        | track Id '('')' MaybeType Block               {% createFuntionEntry id_token $2 $5 [] $6 }
                        | main '(' ')' Block                            {% createFuntionEntry id_token (AST.Id $1) Nothing [] $4 }

ListaParam              :: { [AST.VarDeclaration] }
ListaParam              : ParamDeclaration                        { [$1] }
                        | ListaParam ',' ParamDeclaration           { $3 : $1 }

ParamDeclaration        :: { AST.VarDeclaration }
ParamDeclaration        : Id ':' Type                           {% createParamEntry $1 $3}

Id                      :: { AST.Id }
Id                      : id                                    { AST.Id $1 }

MaybeType               :: { Maybe AST.Type }
MaybeType               : {- empty -}                           { Nothing }
                        | ':' Type                              { Just $2 }

Block                   :: { AST.Block }
Block                   : '{' Seq '}'                           {% incrementScope >> return (AST.Block $ reverse $2) }

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
SimpleInstruction       : Block                                 { AST.BlockInst $1 }
                        | Loop                                  { $1 }
                        | '>>'                                  { AST.NextInst }
                        | '|]'                                  { AST.StopInst }
                        | ChordLegato                           { $1 }
                        | Return                                { $1 }
                        | Statement '|'                         { $1 }

Statement               :: { AST.Instruction }
Statement               : VarDeclaration                        { $1 }
                        | VarInit                               { AST.VarDecInst $1 }
                        | Asignacion                            { $1 }
                        | IO                                    { $1 }
                        | free Id                               {% analyzeVar (AST.id_token $2) >>= return (AST.FreeInst $2) }
                        | LValue '#'                            { AST.SharpExp $1 }
                        | LValue '&'                            { AST.FlatExp $1 }


VarDeclaration          :: { AST.VarDeclaration }
VarDeclaration          : Id ':' Type                           {% createVarEntry $1 $3}
          

VarInit                 :: { AST.VarDeclaration }
VarInit                 : Id ':' Type '<->' Expression          {% createVarEntry $1 $3 $4 >> return (AST.VarDec $1 $3 $4) } -- AST.VarDec

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
Loop                    : loop Id MaybeType Block in '(' Expression ')'                                       {AST.ForInst $2 $3 $4 Nothing $7 Nothing }
                        | loop Id MaybeType Block in '(' Expression ',' Expression ')'                        {AST.ForInst $2 $3 $4 (Just $7) $9 Nothing }
                        | loop Id MaybeType Block in '(' Expression ',' Expression ',' Expression ')'         {AST.ForInst $2 $3 $4 (Just $7) $9 (Just $11) }
                        | loop '(' Expression ')' Block                                                       {AST.WhileInst $3 $5 }

CallFuncion             :: { AST.Expression }
CallFuncion             : play Id with '(' ListExp ')'          {% analyzeVar (AST.id_token $2) >> return (AST.CallExp $2 $ reverse $5) }

ListaVar                :: { [AST.VarDeclaration] }
ListaVar                : VarDeclaration                        { [$1] }
                        | ListaVar ',' VarDeclaration           { $3 : $1 }             

ListExp                 :: { [AST.Expression] }
ListExp                 : Expression                            { [$1] }
                        | ListExp ',' Expression                { $3 : $1 }

Indexing                :: { AST.Expression }
Indexing                : LValue '[' Expression ']'             { AST.IndexingExp $1 $3 $2 }

DotExpression           :: { AST.Expression }
DotExpression           : LValue '.' Id                         {% analyzeLValue (AST.DotExp $1 $3) >> return (AST.DotExp $1 $3) }

Dereference             :: { AST.Expression }
Dereference             : LValue '!'                            { AST.DereferenceExp $1 }

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

ChordLegato             :: { AST.Instruction }
ChordLegato             : chord IdType ChordLegatoFields                  {% createTypeEntry $2 }
                        | legato IdType ChordLegatoFields                 {% createTypeEntry $2 }

ChordLegatoFields          : '{' ListaField '}'               { (reverse $2)}

ListaField              :: { [AST.VarDeclaration] }
ListaField              : FieldDeclaration                        { [$1] }
                        | ListaField ',' FieldDeclaration           { $3 : $1 }

FieldDeclaration        :: { AST.VarDeclaration }
FieldDeclaration        : Id ':' Type                           {% createFieldEntry $1 $3}

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
createFuntionEntry :: Token -> Maybe SemData.Type -> [a] -> b -> ParserMonad ()
createFuntionEntry s t xs i= do
    let funcat = SemData.Function { SemData.ast_function = AST.FunctionDec s t xs i }
    (_, _, lvl) <- RWS.get
    let entry = SemData.Entry {
        SemData.entry_name = s,
        SemData.entry_category = funcat,
        SemData.entry_scope = lvl,
        SemData.entry_type = t,
        SemData.entry_level = Nothing
        }

    insertEntry entry


createVarEntry :: Token -> Maybe SemData.Type -> ParserMonad ()
createVarEntry s t e = do
    (_, _, lvl) <- RWS.get
    -- Lookup para el entry de del tipo
    let entry = SemData.Entry {
        SemData.entry_name = s,
        SemData.entry_category = SemData.Var,
        SemData.entry_scope = lvl,
        SemData.entry_type = t,
        SemData.entry_level = Nothing
    }
    insertEntry entry


createTypeEntry :: Token -> ParserMonad ()
createTypeEntry s = do
    incrementScope
    (_, _, lvl) <- RWS.get
    let entry = SemData.Entry {
        SemData.entry_name = s,
        SemData.entry_category = SemData.Type,
        SemData.entry_scope = lvl,
        SemData.entry_type = Nothing,
        SemData.entry_level = Nothing
    }
    insertEntry entry

createFieldEntry :: Token -> Maybe SemData.Type -> ParserMonad ()
createFieldEntry tk typ = do
    (_, _, lvl) <- RWS.get
    let entry = SemData.Entry {
        SemData.entry_name = tk,
        SemData.entry_category = SemData.Field,
        SemData.entry_scope = lvl,
        SemData.entry_type = typ,
        SemData.entry_level = Nothing
    }
    insertEntry entry

createParamEntry :: Token -> Maybe SemData.Type -> ParserMonad ()
createParamEntry tk typ = do
    (_, _, lvl) <- RWS.get
    let entry = SemData.Entry {
        SemData.entry_name = tk,
        SemData.entry_category = SemData.Param,
        SemData.entry_scope = lvl,
        SemData.entry_type = typ,
        SemData.entry_level = Nothing
    }
    insertEntry entry
    
}



    
