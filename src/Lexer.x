{
module Lexer (Token(..), Rest(..), AlexPosn(..), alexScanTokens) where 
}

%wrapper "posn"

-- Character sets and regular expressions
$digits = 0-9
$alpha = [a-zA-Z]

@string     = \"($printable # \")+\"              -- "strings
@char     = \'($printable # \")+\'                -- "chars

@commentcontent = (\/[^\*]|[^\/]|\n)*
@Eighththrests = (\*\/@commentcontent\/\*)
@Sixteenthrests = (\*\*\/@commentcontent\/\*\*)
@ThirtySecondrests = (\*\*\*\/@commentcontent\/\*\*\*)
@SixtyFourthrests = (\*\*\*\*\/@commentcontent\/\*\*\*\*)


tokens :-

    $white+                             ;

    -- Silencios
    \-\-.*                                    {\p s -> RestToken { token=s, rest=HalfRestToken, posn=p } }
    \~.*                                      {\p s -> RestToken { token=s, rest=QuarterRestToken, posn=p } }
    @Eighththrests                            {\p s -> RestToken { token=s, rest=EightRestToken, posn=p } }
    @Sixteenthrests                           {\p s -> RestToken { token=s, rest=SixteenthRestToken, posn=p } }
    @ThirtySecondrests                        {\p s -> RestToken { token=s, rest=ThirtySecondRestToken, posn=p } }
    @SixtyFourthrests                         {\p s -> RestToken { token=s, rest=SixtyFourthRestToken, posn=p } }

    -- Tipos de datos. 
    whole                               {\p s -> WholeToken s p} -- :: AlexPosn -> String -> Token
    half                                {\p s -> HalfToken s p}
    quarter                             {\p s -> QuarterToken s p}
    eight                               {\p s -> EightToken s p}
    ThirtySecond                        {\p s -> ThirtySecondToken s p}
    SixtyFourth                         {\p s -> SixtyFourthToken s p}
    Melody                              {\p s -> MelodyToken s p}
    Sample                              {\p s -> SampleToken s p}

    -- Instrucciones

    -- Asignacion
    "<->"                               {\p s -> AssignToken s p}

    -- Bloque
    "{"                                 {\p s -> CurlyBracketOpenToken s p}
    "}"                                 {\p s -> CurlyBracketCloseToken s p}
    "|"                                 {\p s -> BarToken s p}

    -- IO
    "("                                 {\p s -> ParenthesisOpenToken s p}
    ")"                                 {\p s -> ParenthesisCloseToken s p}
    "@"                                 {\p s -> RecordToken s p}
    "|>"                                {\p s -> PlaySymToken s p}

    -- Condicionales
    if                                  {\p s -> IfToken s p}
    else                                {\p s -> ElseToken s p}

    -- Ciclos
    loop                                {\p s -> LoopToken s p}
    ":"                                 {\p s -> ColonToken s p}
    in                                  {\p s -> InToken s p}
    ","                                 {\p s -> CommaToken s p}
    -- Stop y Next
    ">>"                                {\p s -> NextToken s p}
    "|]"                                {\p s -> StopToken s p}

    -- Bemoles y Sostenidos
    "#"                                 {\p s -> SharpToken s p}
    "&"                                 {\p s -> FlatToken s p}

    -- Tracks
    track                               {\p s -> TrackToken s p}
    "||"                                {\p s -> DoubleBarToken s p}
    play                                {\p s -> PlayToken s p}
    with                                {\p s -> WithToken s p}

    -- New y Free
    new                                 {\p s -> NewToken s p}
    free                                {\p s -> FreeToken s p}

    -- Chords y Legatos
    chord                               {\p s -> ChordToken s p}
    legato                              {\p s -> LegatoToken s p}

    -- Operadores
    --logicos
    not                                 {\p s -> NotToken s p}
    and                                 {\p s -> AndToken s p}
    or                                  {\p s -> OrToken s p}

    -- aritmeticos
    "-"                                 {\p s -> MinusToken s p}
    mod                                 {\p s -> ModToken s p}
    "/"                                 {\p s -> DivToken s p}
    "*"                                 {\p s -> MultToken s p}
    "^"                                 {\p s -> PowToken s p}
    "+"                                 {\p s -> PlusToken s p}

    -- comparacion
    "="                                 {\p s -> EqualToken s p}
    "/="                                {\p s -> NotEqualToken s p}
    "<"                                 {\p s -> LessToken s p}
    ">"                                 {\p s -> GreaterToken s p}
    "<="                                {\p s -> LessEqualToken s p}
    ">="                                {\p s -> GreaterEqualToken s p}

    -- indexacion
    "["                                 {\p s -> BracketOpenToken s p}
    "]"                                 {\p s -> BracketCloseToken s p}

    -- Acceso a Chords
    "."                                 {\p s -> DotToken s p}

    -- Literales
    $digits+                            {\p s -> IntToken s p (read s) }
    $digits+\.$digits+                  {\p s -> FloatToken s p (read s) }
    @string                             {\p s -> StringToken s p (read s) }
    @char                               {\p s -> CharToken s p (read s)}
    maj                                 {\p s -> MajToken s p}
    min                                 {\p s -> MinToken s p}

    -- ID
    [a-zA-Z][a-zA-Z0-9\_]*\'*             {\p s -> IdToken s p}

    "=="                                {\p _ -> didYouMeanToken p "="}
    "!="                                {\p _ -> didYouMeanToken p "/="}
    "<-"                                {\p _ -> didYouMeanToken p "<->"}

{

didYouMeanToken :: AlexPosn -> String -> a
didYouMeanToken p op = error $ "Did you men " ++ op ++ " at line: " ++ show (posnLine p) ++ ", col: " ++ show (posnCol p) ++ "?"

posnLine :: AlexPosn -> Int
posnLine (AlexPn _ l _) = l

posnCol :: AlexPosn -> Int
posnCol (AlexPn _ _ c) = c

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
    WholeToken { token :: String, posn :: AlexPosn } |
    HalfToken { token :: String, posn :: AlexPosn } |
    QuarterToken { token :: String, posn :: AlexPosn } |
    EightToken { token :: String, posn :: AlexPosn } |
    ThirtySecondToken { token :: String, posn :: AlexPosn } |
    SixtyFourthToken { token :: String, posn :: AlexPosn } |
    MelodyToken { token :: String, posn :: AlexPosn } |
    SampleToken { token :: String, posn :: AlexPosn } |
    
    -- Instrucciones
    AssignToken { token :: String, posn :: AlexPosn } |
    CurlyBracketOpenToken { token :: String, posn :: AlexPosn } |
    CurlyBracketCloseToken { token :: String, posn :: AlexPosn } |
    BarToken { token :: String, posn :: AlexPosn } |
    ParenthesisOpenToken { token :: String, posn :: AlexPosn } |
    ParenthesisCloseToken { token :: String, posn :: AlexPosn } |
    RecordToken { token :: String, posn :: AlexPosn } |
    PlaySymToken { token :: String, posn :: AlexPosn } |
    IfToken { token :: String, posn :: AlexPosn } |
    ElseToken { token :: String, posn :: AlexPosn } |
    LoopToken { token :: String, posn :: AlexPosn } |
    ColonToken { token :: String, posn :: AlexPosn } |
    InToken { token :: String, posn :: AlexPosn } |
    CommaToken { token :: String, posn :: AlexPosn } |
    NextToken { token :: String, posn :: AlexPosn } |
    StopToken { token :: String, posn :: AlexPosn } |
    SharpToken { token :: String, posn :: AlexPosn } |
    FlatToken { token :: String, posn :: AlexPosn } |
    TrackToken { token :: String, posn :: AlexPosn } |
    DoubleBarToken { token :: String, posn :: AlexPosn } |
    PlayToken { token :: String, posn :: AlexPosn } |
    WithToken { token :: String, posn :: AlexPosn } |
    NewToken { token :: String, posn :: AlexPosn } |
    FreeToken { token :: String, posn :: AlexPosn } |
    RestToken { token :: String, posn :: AlexPosn, rest :: Rest } |
    ChordToken { token :: String, posn :: AlexPosn } |
    LegatoToken { token :: String, posn :: AlexPosn } |

    -- Operadores
    NotToken { token :: String, posn :: AlexPosn } |
    AndToken { token :: String, posn :: AlexPosn } |
    OrToken { token :: String, posn :: AlexPosn } |
    MinusToken { token :: String, posn :: AlexPosn } |
    ModToken { token :: String, posn :: AlexPosn } |
    DivToken { token :: String, posn :: AlexPosn } |
    MultToken { token :: String, posn :: AlexPosn } |
    PowToken { token :: String, posn :: AlexPosn } |
    PlusToken { token :: String, posn :: AlexPosn } |
    EqualToken { token :: String, posn :: AlexPosn } |
    NotEqualToken { token :: String, posn :: AlexPosn } |
    LessToken { token :: String, posn :: AlexPosn } |
    GreaterToken { token :: String, posn :: AlexPosn } |
    LessEqualToken { token :: String, posn :: AlexPosn } |
    GreaterEqualToken { token :: String, posn :: AlexPosn } |
    BracketOpenToken { token :: String, posn :: AlexPosn } |
    BracketCloseToken { token :: String, posn :: AlexPosn } |
    DotToken { token :: String, posn :: AlexPosn } |

    -- Identificador
    IdToken { token :: String, posn :: AlexPosn} |

    -- Literales
    IntToken { token :: String, posn :: AlexPosn, intLiteral :: Int} |
    FloatToken { token :: String, posn :: AlexPosn, floatLiteral :: Float} |
    MajToken { token :: String, posn :: AlexPosn } |
    MinToken { token :: String, posn :: AlexPosn } |
    StringToken { token :: String, posn :: AlexPosn, stringLiteral :: String} |
    CharToken { token :: String, posn :: AlexPosn, charLiteral :: Char}

    deriving (Eq, Show)

}