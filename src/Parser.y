{
module Parser(parse) where
import Lexer
import Tokens
import ParseError
import qualified AST
}

%name alexParse
%error { parseError }
%monad { Alex }
%lexer { lexerWrapper } { EOFToken }
%tokentype { Token }

%token
    whole               { WholeToken _ _ _ }
    half                { HalfToken _ _ _ }
    quarter             { QuarterToken _ _ _ }
    eighth              { EightToken _ _ _ }
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
FunctionDeclaration     : track Id '(' ListaVar ')' MaybeType Block   { AST.FunctionDec $2 $6 (reverse $4) $7 }
                        | track Id '('')' MaybeType Block               { AST.FunctionDec $2 $5 [] $6 }

Id                      :: { AST.Id }
Id                      : id                                    { AST.Id $1 }

MaybeType               :: { Maybe AST.Type }
MaybeType               : {- empty -}                           { Nothing }
                        | ':' Type                              { Just $2 }

Block                   :: { AST.Block }
Block                   : '{' Seq '}'                           { AST.Block $ reverse $2 }

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
Statement               : VarDeclaration                        { AST.VarDecInst $1 }
                        | Asignacion                            { $1 }
                        | IO                                    { $1 }
                        | free Id                               { AST.FreeInst $2 }
                        | LValue '#'                            { AST.SharpExp $1 }
                        | LValue '&'                            { AST.FlatExp $1 }


VarDeclaration          :: { AST.VarDeclaration }
VarDeclaration          : Id ':' Type MaybeAsignar              { AST.VarDec $1 $3 $4 }

MaybeAsignar            :: { Maybe AST.Expression }    
MaybeAsignar            : '<->' Expression                      { Just $2 }
                        | {- empty -}                           { Nothing }

Asignacion              :: { AST.Instruction }
Asignacion              : LValue '<->' Expression               { AST.AssignInst $1 $3 }

LValue                  :: { AST.Expression }
LValue                  : Indexing                              { $1 }
                        | DotExpression                         { $1 }
                        | Dereference                           { $1 }
                        | Id                                    { AST.IdExp $1 }

Return                  :: { AST.Instruction }
Return                  : Expression '||'                       { AST.ReturnInst $1 }

IO                      :: { AST.Instruction }
IO                      : '@' '(' ListExp ')'                   { AST.RecordInst $ reverse $3 }
                        | '|>' '(' ListExp ')'                  { AST.PlayInst $ reverse $3 }

Loop                    :: { AST.Instruction }
Loop                    : loop Id MaybeType Block in '(' Expression ')'                                       { AST.ForInst $2 $3 $4 Nothing $7 Nothing }
                        | loop Id MaybeType Block in '(' Expression ',' Expression ')'                        { AST.ForInst $2 $3 $4 (Just $7) $9 Nothing }
                        | loop Id MaybeType Block in '(' Expression ',' Expression ',' Expression ')'         { AST.ForInst $2 $3 $4 (Just $7) $9 (Just $11) }
                        | loop '(' Expression ')' Block                                                       { AST.WhileInst $3 $5 }

CallFuncion             :: { AST.Expression }
CallFuncion             : play Id with '(' ListExp ')'          { AST.CallExp $2 $ reverse $5 }

ListaVar                :: { [AST.VarDeclaration] }
ListaVar                : VarDeclaration                        { [$1] }
                        | ListaVar ',' VarDeclaration           { $3 : $1 }             

ListExp                 :: { [AST.Expression] }
ListExp                 : Expression                            { [$1] }
                        | ListExp ',' Expression                { $3 : $1 }

Indexing                :: { AST.Expression }
Indexing                : Expression '[' Expression ']'             { AST.IndexingExp $1 $3 }

DotExpression           :: { AST.Expression }
DotExpression           : Expression '.' Id                         { AST.DotExp $1 $3 }

Dereference             :: { AST.Expression }
Dereference             : Expression '!'                            { AST.DereferenceExp $1 }

Type                    :: { AST.Type }
Type                    : whole                                 { AST.Type $1 Nothing }
                        | half                                  { AST.Type $1 Nothing }
                        | quarter                               { AST.Type $1 Nothing }
                        | eighth                                { AST.Type $1 Nothing }
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
LiteralMelody           : '[' ListExp ']'                           { AST.LiteralMelody $2 }

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
ChordLegato             : chord ChordLegatoAux                  { AST.ChordDec $2 }
                        | legato ChordLegatoAux                 { AST.LegatoDec $2 }

ChordLegatoAux          : IdType '{' ListaVar '}'               { AST.ParamsCL $1 (reverse $3)}

{
parse :: String -> Either String AST.Program
parse s = runAlex s alexParse
}