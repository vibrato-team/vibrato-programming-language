{
module Frontend.Parser.PreParser(preparse) where
import Frontend.Parser.Parser
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
}

%name preparse
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
FunctionDeclaration     : Signature Block PopScope                          {}
MainDeclaration         : main PushScope '(' ClosePar Block                      {}

--------------------------------------------------------------------------------
--------------------------For error recovery------------------------------------
--------------------------------------------------------------------------------

OpenBracket             : '{'       { }

CloseBracket             : '}'       { }
                        | error     { }

ClosePar                : ')'         { True }
                        | error         { False }

CloseAngular            : '>'       { True }
                        | error     { False }

CloseSquare             : ']'       { True }
                        | error     { False }

With                    : with                  { True }
                        | error                 { False }

In                      : in        { True }
                        | error     { False }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Signature               : track Id PushScope '(' ListaParam ClosePar MaybeType             {% do
                                                                                            let tk = AST.id_token $2
                                                                                            PMonad.pushFunctionName $ token tk
                                                                                            createFunctionEntry tk $7 $2 (reverse $5) Nothing Nothing }

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

MaybeType               :: { Maybe AST.ASTType }
MaybeType               : {- empty -}                           { Nothing }
                        | ':' Type                              { Just $2 }

Block                   : PushScope OpenBracket Seq CloseBracket PopScope                  { }

Seq                     : Instruction                           { }
                        | Seq Instruction                       { }

Instruction             : OpenCondition                         { }
                        | ClosedCondition                       { }

OpenCondition           : if '(' Expression ClosePar Instruction                         { }
                        | if '(' Expression ClosePar ClosedCondition else OpenCondition  { }

ClosedCondition         : if '(' Expression ClosePar ClosedCondition else ClosedCondition    { }
                        | SimpleInstruction                                             { }

SimpleInstruction       : Block                                 { }
                        | Loop                                  { }
                        | '>>'                                  { }
                        | '|]'                                  { }
                        | Return                                { }
                        | Statement '|'                         { }

Statement               : VarDeclaration                        { }
                        | VarInit                               { }
                        | Asignacion                            { }
                        | IO                                    { }
                        | free Expression                       { }
                        | LValue '#'                            { }
                        | LValue '&'                            { }
                        | CallFuncion                           { }


VarDeclaration          : Id ':' Type                           { }

VarInit                 : Id ':' Type '<->' Expression          { }

Asignacion              : LValue '<->' Expression               { }

LValue                  : Indexing                              { }
                        | DotExpression                         { }
                        | Dereference                           { }
                        | Id                                    { }

Return                  : Expression '||'                       { }
                        | '||'                                  { }

IO                      : '@' '(' ListExp ClosePar                   { }
                        | '|>' '(' ListExp ClosePar                  { }

Loop                    : loop PushScope Id MaybeType Block PopScope In '(' Expression ClosePar                                       { }
                        | loop PushScope Id MaybeType Block PopScope In '(' Expression ',' Expression ClosePar                        { }
                        | loop PushScope Id MaybeType Block PopScope In '(' Expression ',' Expression ',' Expression ClosePar         { }
                        | loop '(' Expression ClosePar Block                                                                          { }

CallFuncion             : play Id With '(' ListExp ClosePar          { }

                        | play Id                              { }

ListExp                 : Expression                            { }
                        | ListExp ',' Expression                { }
                        | {-empty-}                             { }

Indexing                : Expression '[' Expression CloseSquare             { }

DotExpression           : Expression '.' Id                         { }

Dereference             : Expression '!'                            { }

Type                    :: { AST.ASTType }
Type                    : PrimitiveType                         { $1 }
                        | MelodyType                            { $1 }
                        | sample '<' Type CloseAngular          { AST.Compound (token $1) $3 }
                        | AlreadyDeclaredIdType                 { $1 }

PrimitiveType           :: { AST.ASTType }
PrimitiveType           : whole                                 { AST.Simple (token $1) }
                        | half                                  { AST.Simple (token $1) }
                        | quarter                               { AST.Simple (token $1) }
                        | eighth                                { AST.Simple (token $1) }
                        | ThirtySecond                          { AST.Simple (token $1) }
                        | SixtyFourth                           { AST.Simple (token $1) }

MelodyType              :: { AST.ASTType }
MelodyType              : melody '<' Type CloseAngular          { AST.Compound (token $1) $3 }

Literal                 : int                                   { }
                        | float                                 { }
                        | string                                { }
                        | char                                  { }
                        | MelodyLiteral                         { }

                        | IdType '(' ListExp ClosePar           { }

                        | IdType '(' Id ':' Expression ClosePar     { }

                        | PrimitiveType '(' Expression ClosePar     { }

                        | MelodyType '(' Expression ClosePar        { }

MelodyLiteral           : '[' ListExp CloseSquare                       { }

Expression              : LValue %prec LVALUE                   { }
                        -- Boolean
                        | not Expression                        { }
                        | Expression and Expression             { } 
                        | Expression or Expression              { } 

                        -- Aritmetivos
                        | '-' Expression %prec NEG              { }

                        | Expression '-' Expression             { }
                        | Expression mod Expression             { }
                        | Expression '/' Expression             { }
                        | Expression '*' Expression             { }
                        | Expression '^' Expression             { }             
                        | Expression '+' Expression             { }

                        -- Relacionales
                        | Expression '=' Expression             { }

                        | Expression '/=' Expression            { }
                        | Expression '<' Expression             { }
                        | Expression '>' Expression             { }
                        | Expression '<=' Expression            { }
                        | Expression '>=' Expression            { }

                        -- Micelaneos
                        | Literal                               { }
                        | '(' Expression ClosePar                    { }

                        | new Literal                           { }
                        | new IdType                            { }

                        | length '(' Expression ClosePar        { }

                        | CallFuncion                           { }

IdType                  :: { AST.ASTType }
IdType                  : id_type                               { AST.Simple (token $1) }

AlreadyDeclaredIdType   :: { AST.ASTType }
AlreadyDeclaredIdType   : id_type                               {%do
                                                                    entryMaybe <- PMonad.lookup (token $1)
                                                                    case entryMaybe of
                                                                        Nothing -> do
                                                                            return AST.errorType
                                                                        Just _ -> return $ AST.Simple (token $1) }

NewType                 : chord IdType                         {% createTypeEntry $1 (AST.type_str $2) }
                        | legato IdType                        {% createTypeEntry $1 (AST.type_str $2) }

ChordLegato             : NewType PushScope ChordLegatoFields PopScope  { }

ChordLegatoFields       : OpenBracket ListaField CloseBracket                    { }

ListaField              : FieldDeclaration                      { }
                        | ListaField ',' FieldDeclaration       { }

FieldDeclaration        : Id ':' Type                           { }

PushScope               :: { () }
PushScope               : {- empty -}                           {% PMonad.pushScope }

PopScope                :: { () }
PopScope                : {- empty -}                           {% PMonad.popScope }

{
--------------------------------------------
----------------- END ----------------------
--------------------------------------------
}