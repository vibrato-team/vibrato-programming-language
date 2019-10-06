{
module Parser(parse) where
import Lexer
import Tokens
}

%name parse
%tokentype { Token }
%error { parseError }
-- TODO: %lexer { lexer } { EOFToken }. Hace falta una función con la firma lexer :: (Token -> Alex a) -> Alex a
%monad { Alex }

%token
    whole               { WholeToken }
    half                { HalfToken }
    quarter             { QuarterToken }
    eighth              { EighthToken }
    32th                { ThirtySecondToken }
    64th                { SixtyFourthToken }
    Melody              { MelodyToken }
    Sample              { SampleToken }

    '<->'               { AssignToken }
    '{'                 { OpenCurlyToken }
    '}'                 { CloseCurlyToken }
    '|'                 { BarToken }
    
    '('                 { OpenParToken }
    ')'                 { CloseParToken }
    '@'                 { RecordToken }
    '|>'                { PlaySymToken }

    if                  { IfToken }
    else                { ElseToken }

    loop                { LoopToken }
    ':'                 { ColonToken }
    in                  { InToken }
    ','                 { CommaToken }

    '>>'                { NextToken }
    '|]'                { StopToken }

    '#'                 { SharpToken }
    '&'                 { FlatToken }

    track               { TrackToken }
    '||'                { DoubleBarToken }
    play                { PlayToken }
    with                { WithToken }

    new                 { NewToken }
    free                { FreeToken }

    rest                { RestToken }

    chord               { ChordToken }
    legato              { LegatoToken }

    '!'                 { DereferenceToken }

    not                 { NotToken }
    and                 { AndToken }
    or                  { OrToken }

    '-'                 { MinusToken }
    'mod'               { ModToken }
    '/'                 { DivToken }
    '*'                 { MultToken }
    '^'                 { PowToken }
    '+'                 { PlusToken }

    '='                 { EqualToken }
    '/='                { NotEqualToken }
    '<'                 { LessToken }
    '>'                 { GreaterToken }
    '<='                { LessEqualToken }
    '>='                { GreaterEqualToken }

    '['                 { BracketOpenToken }
    ']'                 { BracketCloseToken }

    '.'                 { DotToken }

    int                 { IntToken }
    float               { FloatToken }
    maj                 { MajToken }
    min                 { MinToken }
    string              { StringToken }
    char                { CharToken }

    id                  { IdToken }

%%