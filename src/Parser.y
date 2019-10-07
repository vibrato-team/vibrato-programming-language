{
module Parser(parse) where
import Lexer
import Tokens
import qualified AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    whole               { WholeToken _ _ _ }
    half                { HalfToken _ _ _ }
    quarter             { QuarterToken _ _ _ }
    eighth              { EighthToken _ _ _ }
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

    rest                { RestToken _ _ _ _ }

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

%%

Start                   :: { AST.Program }
Start                   : ExternalList                          { AST.Start $ reverse $1 }

ExternalList            :: { [AST.ExternalDeclaration] }
ExternalList            : ExternalDeclaration                   { [$1] }
                        | ExternalList ExternalDeclaration      { $2 : $1 }

ExternalDeclaration     :: { AST.ExternalDeclaration }
ExternalDeclaration     : FunctionDeclaration                   { AST.ExternalFunctionDeclaration $1 }
                        | VarDeclaration                        { AST.ExternalVarDeclaration $1 }

FunctionDeclaration     :: { AST.FunctionDeclaration }
FunctionDeclaration     : track Id '(' ListaVar ')' MaybeType Block   { AST.FuncDec $2 $6 (reverse $4) $7 }

Id                      :: { AST.Id }
Id                      : id                                    { AST.Id $1 }

MaybeType               :: { Maybe AST.Type }
MaybeType               : {- empty -}                           { Nothing }
                        | ':' Type                              { Just $1 }

Block                   :: { AST.Block }
Block                   : '{' Seq '}'                           { AST.Block $ reverse $2 }

Seq                     :: { [AST.Instruction] }
Seq                     : Instruction                           { $1 }
                        | Seq '|' Instruction                   { $3 : $1 }

Instruction             :: { AST.Instruction }
Instruction             : OpenCondition                         { $1 }
                        | ClosedCondition                       { $1 }

SimpleInstruction       :: { AST.Instruction }
SimpleInstruction       : VarDeclaration                        { AST.VarDecInst $1 }
                        | Asignacion                            { $1 }
                        | Return                                { $1 }
                        | Block                                 { AST.BlockInst $1 }
                        | IO                                    { $1 }
                        | Loop                                  { $1 }
                        | '>>'                                  { $1 }
                        | '|]'                                  { $1 }
                        | free id                               { $1 }

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

OpenCondition           :: { AST.Instruction }
OpenCondition           : if '(' Expression ')' Instruction                         { AST.IfInst $3 $5 Nothing }
                        | if '(' Expression ')' ClosedCondition else OpenCondition  { AST.IfInst $3 $5 (Just $7) }

ClosedCondition         :: { AST.Instruction }
ClosedCondition         : if '(' Expression ')' ClosedCondition else ClosedCondition    { AST.IfInst $3 $5 (Just $7) }
                        | SimpleInstruction                                             { $1 }

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
Indexing                : LValue '[' Expression ']'             { AST.IndexingExp $1 $3 }

DotExpression           :: { AST.Expression }
DotExpression           : LValue '.' Id                         { AST.DotExp $1 $3 }

Dereference             :: { AST.Expression }
Dereference             : LValue '!'                            { AST.DereferenceExp $1 }

Type                    :: { AST.Type }
Type                    : whole                                 { AST.Type $1 Nothing }
                        | half                                  { AST.Type $1 Nothing }
                        | quarter                               { AST.Type $1 Nothing }
                        | eighth                                { AST.Type $1 Nothing }
                        | ThirtySecond                                  { AST.Type $1 Nothing }
                        | SixtyFourth                                  { AST.Type $1 Nothing }
                        | melody '<' Type '>'                   { AST.Type $1 (Just $3) }
                        | sample '<' Type '>'                   { AST.Type $1 (Just $3) }
                        | id                                    { AST.Type $1 Nothing }

Literal                 :: { AST.Expression }
Literal                 : int                                   { AST.Literal $1 }
                        | float                                 { AST.Literal $1 }
                        | maj                                   { AST.Literal $1 }
                        | min                                   { AST.Literal $1 }
                        | string                                { AST.Literal $1 }
                        | char                                  { AST.Literal $1 }
                        | LiteralMelody                         { $1 }

LiteralMelody           :: { AST.Expression }
LiteralMelody           : '[' ListExp ']'                           { AST.LiteralMelody $2 Nothing Nothing }
                        | melody '<' Type '>' '(' Expression ')'    { AST.LiteralMelody [] (Just $6) (Just $3) }

Expression              : LValue                                { $1 }
--             | not Expression

--           | Expression and Expression
--           | Expression or Expression
          
--           -- Aritmetivos
--           | '-' Expression
--           | Expression '-' Expression
--           | Expression mod Expression
--           | Expression '/' Expression
--           | Expression '*' Expression
--           | Expression '^' Expression
--           | Expression '+' Expression
  
--           -- Relacionales
--           | Expression '=' Expression
--           | Expression '/=' Expression
--           | Expression '<' Expression
--           | Expression '>' Expression
--           | Expression '<=' Expression
--           | Expression '>=' Expression

--           -- Acordes y legato
--           | DotExpression
  
          -- Acordes y legato
--          | Expresion '.' Expresion

          -- Para Samples
--          | Expresion '!'
--           -- Micelaneos
--           | Literal
--           | '(' Expression ')'
  
--           -- Sostenidos y bemoles
--           | Expression '#'
--           | Expression '&'
  
--           | NewToken Type '(' Expression ')'

--           | CallFuncion                           { $1 }

-- ChordLegato : chord ChordLegatoAux
--             | legato ChordLegatoAux

-- ChordLegatoAux : id '{' ListaVarCL '}'

-- ListaVarCL : id ':' Tipo
--          | ListaVar '|' id ':' Tipo
