{
    module Lexer (Token(..), Rest(..), AlexPosn(..), alexScanTokens) where 
}

%wrapper "posn"

-- Character sets and regular expressions
$digits = 0-9
$alpha = [a-zA-Z] 
@string     = \" ($printable # \")* \"              -- "strings
@char     = \' ($printable # \")* \'                -- "chars
@Eighththrests = (\*{1}\/ (\/[^\*]|[^\/])* \/\*{1})
@Sixteenthrests = (\*{2}\/ (\/[^\*]|[^\/])* \/\*{2})
@ThirtySecondrests = (\*{3}\/ (\/[^\*]|[^\/])* \/\*{3})
@SixtyFourthrests = (\*{4}\/ (\/[^\*]|[^\/])* \/\*{4})


tokens :-

    $white+                             ;

    -- Tipos de datos. 
    whole                               {\p s -> WholeToken p} -- :: AlexPosn -> String -> Token
    half                                {\p s -> HalfToken p}
    quarter                             {\p s -> QuarterToken p}
    eight                               {\p s -> EightToken p}
    ThirtySecond                        {\p s -> ThirtySecondToken p}
    SixtyFourth                         {\p s -> SixtyFourthToken p}
    Melody                              {\p s -> MelodyToken p}
    Sample                              {\p s -> SampleToken p}

    -- Instrucciones

    -- Asignacion
    "<->"                               {\p s -> AssignToken p}

    -- Bloque
    "{"                                 {\p s -> CurlyBracketOpenToken p}
    "}"                                 {\p s -> CurlyBracketCloseToken p}
    "|"                                 {\p s -> BarToken p}

    -- IO
    "("                                 {\p s -> ParenthesisOpenToken p}
    ")"                                 {\p s -> ParenthesisCloseToken p}
    "@"                                 {\p s -> RecordToken p}
    "|>"                                {\p s -> PlaySymToken p}

    -- Condicionales
    if                                  {\p s -> IfToken p}
    else                                {\p s -> ElseToken p}

    -- Ciclos
    loop                                {\p s -> LoopToken p}
    ":"                                 {\p s -> ColonToken p}
    in                                  {\p s -> InToken p}
    ","                                 {\p s -> CommaToken p}
    -- Stop y Next
    ">>"                                {\p s -> NextToken p}
    "|]"                                {\p s -> StopToken p}

    -- Bemoles y Sostenidos
    "#"                                 {\p s -> SharpToken p}
    "&"                                 {\p s -> FlatToken p}

    -- Tracks
    track                               {\p s -> TrackToken p}
    "||"                                {\p s -> DoubleBarToken p}
    play                                {\p s -> PlayToken p}
    with                                {\p s -> WithToken p}

    -- New y Free
    new                                 {\p s -> NewToken p}
    free                                {\p s -> FreeToken p}

    -- Silencios
    \-\-.*                                    {\p _ -> RestToken { rest=HalfRestToken, posn=p } }
    \~.*                                      {\p _ -> RestToken { rest=QuarterRestToken, posn=p } }
    @Eighththrests                            {\p _ -> RestToken { rest=EightRestToken, posn=p } }
    @Sixteenthrests                           {\p _ -> RestToken { rest=SixteenthRestToken, posn=p } }
    @ThirtySecondrests                        {\p _ -> RestToken { rest=ThirtySecondRestToken, posn=p } }
    @SixtyFourthrests                         {\p _ -> RestToken { rest=SixtyFourthRestToken, posn=p } }


    -- Chords y Legatos
    chord                               {\p s -> ChordToken p}
    legato                              {\p s -> LegatoToken p}

    -- Operadores
    --logicos
    not                                 {\p s -> NotToken p}
    and                                 {\p s -> AndToken p}
    or                                  {\p s -> OrToken p}

    -- aritmeticos
    "-"                                 {\p s -> MinusToken p}
    mod                                 {\p s -> ModToken p}
    "/"                                 {\p s -> DivToken p}
    "*"                                 {\p s -> MultToken p}
    "^"                                {\p s -> PowToken p}
    "+"                                 {\p s -> PlusToken p}

    -- comparacion
    "="                                 {\p s -> EqualToken p}
    "/="                                {\p s -> NotEqualToken p}
    "<"                                 {\p s -> LessToken p}
    ">"                                 {\p s -> GreaterToken p}
    "<="                                {\p s -> LessEqualToken p}
    ">="                                {\p s -> GreaterEqualToken p}

    -- indexacion
    "["                                 {\p s -> BracketOpenToken p}
    "]"                                 {\p s -> BracketCloseToken p}

    -- Acceso a Chords
    "."                                 {\p s -> DotToken p}

    -- Literales
    $digits+                            {\p s -> IntToken p $ read s }
    $digits+\.$digits+                  {\p s -> FloatToken p $ read s }
    @string                             {\p s -> StringToken p s }
    maj                                 {\p s -> MajToken p}
    min                                 {\p s -> MinToken p}


    -- ID
    [a-zA-Z]([a-zA-Z][0-9]\_)*\'*       {\p s -> IdToken p s}

{
data Rest =
    HalfRestToken|
    QuarterRestToken|
    EightRestToken |
    SixteenthRestToken |
    ThirtySecondRestToken |
    SixtyFourthRestToken
    deriving (Eq, Show)

data Token = 
    -- Tipos de datos
    WholeToken { posn :: AlexPosn } |
    HalfToken { posn :: AlexPosn } |
    QuarterToken { posn :: AlexPosn } |
    EightToken { posn :: AlexPosn } |
    ThirtySecondToken { posn :: AlexPosn } |
    SixtyFourthToken { posn :: AlexPosn } |
    MelodyToken { posn :: AlexPosn } |
    SampleToken { posn :: AlexPosn } |
    
    -- Instrucciones
    AssignToken { posn :: AlexPosn } |
    CurlyBracketOpenToken { posn :: AlexPosn } |
    CurlyBracketCloseToken { posn :: AlexPosn } |
    BarToken { posn :: AlexPosn } |
    ParenthesisOpenToken { posn :: AlexPosn } |
    ParenthesisCloseToken { posn :: AlexPosn } |
    RecordToken { posn :: AlexPosn } |
    PlaySymToken { posn :: AlexPosn } |
    IfToken { posn :: AlexPosn } |
    ElseToken { posn :: AlexPosn } |
    LoopToken { posn :: AlexPosn } |
    ColonToken { posn :: AlexPosn } |
    InToken { posn :: AlexPosn } |
    CommaToken { posn :: AlexPosn } |
    NextToken { posn :: AlexPosn } |
    StopToken { posn :: AlexPosn } |
    SharpToken { posn :: AlexPosn } |
    FlatToken { posn :: AlexPosn } |
    TrackToken { posn :: AlexPosn } |
    DoubleBarToken { posn :: AlexPosn } |
    PlayToken { posn :: AlexPosn } |
    WithToken { posn :: AlexPosn } |
    NewToken { posn :: AlexPosn } |
    FreeToken { posn :: AlexPosn } |
    RestToken { posn :: AlexPosn, rest :: Rest } |
    ChordToken { posn :: AlexPosn } |
    LegatoToken { posn :: AlexPosn } |

    -- Operadores
    NotToken { posn :: AlexPosn } |
    AndToken { posn :: AlexPosn } |
    OrToken { posn :: AlexPosn } |
    MinusToken { posn :: AlexPosn } |
    ModToken { posn :: AlexPosn } |
    DivToken { posn :: AlexPosn } |
    MultToken { posn :: AlexPosn } |
    PowToken { posn :: AlexPosn } |
    PlusToken { posn :: AlexPosn } |
    EqualToken { posn :: AlexPosn } |
    NotEqualToken { posn :: AlexPosn } |
    LessToken { posn :: AlexPosn } |
    GreaterToken { posn :: AlexPosn } |
    LessEqualToken { posn :: AlexPosn } |
    GreaterEqualToken { posn :: AlexPosn } |
    BracketOpenToken { posn :: AlexPosn } |
    BracketCloseToken { posn :: AlexPosn } |
    DotToken { posn :: AlexPosn } |

    -- Identificador
    IdToken { posn :: AlexPosn, token :: String} |

    -- Literales
    IntToken { posn :: AlexPosn, intLiteral :: Int} |
    FloatToken { posn :: AlexPosn, floatLiteral :: Float} |
    MajToken { posn :: AlexPosn } |
    MinToken { posn :: AlexPosn } |
    StringToken { posn :: AlexPosn, stringLiteral :: String} |
    CharToken { posn :: AlexPosn, charLiteral :: Char}

    deriving (Eq, Show)
        
}




