{
module Parser(parse) where
import Lexer
import Tokens
}

%name parse
%tokentype { Token _ _ _ }
%error { parseError }
-- TODO: %lexer { lexer } { EOFToken _ _ _ }. Hace falta una función con la firma lexer :: (Token -> Alex a) -> Alex a
%monad { Alex }

%token
    whole               { WholeToken _ _ _ }
    half                { HalfToken _ _ _ }
    quarter             { QuarterToken _ _ _ }
    eighth              { EighthToken _ _ _ }
    32th                { ThirtySecondToken _ _ _ }
    64th                { SixtyFourthToken _ _ _ }
    melody              { MelodyToken _ _ _ }
    sample              { SampleToken _ _ _ }

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

inicio :: { inicio }
inicio : Alcance        { $1 }

Alcance :: { Program }
Alcance : Comment
        | Funcion
        | ChordLegato

Funcion :: { Funcion }
Funcion : id '(' ListaVar ')' Block Alcance
        | track id '(' ListaVar ')' Block Alcance


Block : '{' Seq '}'

Seq : Instruccion
    | Seq '|' Instruccion
    | Instruccion '||'
    | Seq Instruccion '||'
    | Comment
    | Seq Comment

Instruccion : Declaracion
            | Asignacion
            | Block
            | IO
            | Condicional
            | Iteracion
            | CallFuncion
            | '>>'
            | '|]'
            | free id
            | ChordLegato

Declaracion : id ':' AsignarAux

Asignacion : id AsignarAux

AsignarAux : '=' Expresion
           | {- empty -}           { [] }

IO : '@' '(' ListExp ')'
   | '|>' '(' ListExp ')'

Condicional : if '(' Expresion ')' Instruccion CondicionElse

CondicionElse : else Instruccion
              | {- empty -}           { [] }

Iteracion : loop id ':' Tipo Instruccion in '(' ListExp ')'
          | loop id Instruccion in '(' ListExp ')'
          | loop '(' Expresion ')' Instruccion

CallFuncion : play id with '(' ListExp ')'

ListaVar : id ':' Tipo
         | ListaVar ',' id ':' Tipo

ListExp : Expresion
        | ListExp ',' Expresion

Tipo : whole
     | half
     | quarter
     | eighth
     | 32th
     | 64th
     | melody '<' Tipo '>'
     | sample '<' Tipo '>'

Literal : int
        | float
        | maj
        | min
        | string
        | char

Expresion : not Expresion
          | Expresion and Expresion
          | Expresion or Expresion
          
          -- Aritmetivos
          | '-' Expresion
          | Expresion '-' Expresion
          | Expresion mod Expresion
          | Expresion '/' Expresion
          | Expresion '*' Expresion
          | Expresion '^' Expresion
          | Expresion '+' Expresion
  
          -- Relacionales
          | Expresion '=' Expresion
          | Expresion '/=' Expresion
          | Expresion '<' Expresion
          | Expresion '>' Expresion
          | Expresion '<=' Expresion
          | Expresion '>=' Expresion
  
          -- Arreglos
          | Expresion '[' Expresion ']'
  
          -- Acordes y legato
          | Expresion '.' Expresion

          -- Para Samples
          | Expresion '!'
  
          -- Micelaneos
          | Variable
          | Literal
          | '(' Expresion ')'
  
          -- Sostenidos y bemoles
          | Expresion '#'
          | Expresion '&'
  
          | NewToken Tipo '(' Expresion ')'

ChordLegato : chord ChordLegatoAux
            | legato ChordLegatoAux

ChordLegatoAux : id '{' ListaVarCL '}'

ListaVarCL : id ':' Tipo
         | ListaVar '|' id ':' Tipo