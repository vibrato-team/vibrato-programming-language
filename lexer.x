{
    module Lexer (Token(..), AlexPosn(..), alexScanTokens, token_posn) where 
}

%wrapper "posn"

-- Character sets and regular expressions
$digit = 0-9
$alpha = [a-zA-Z] 
@string     = \" ($printable # \")* \"              -- "strings


tokens :-

$white+                             ;

-- Tipos de datos. 
whole                               {\p s -> Whole p} -- :: AlexPosn -> String -> Token
half                                {\p s -> Half p}
quarter                             {\p s -> Quarter p}
eight                               {\p s -> Eight p}
32th                                {\p s -> 32th p}
64th                                {\p s -> 64th p}
Melody                              {\p s -> Melody p}
Sample                              {\p s -> Sample p}

-- Instrucciones

-- Asignacion
"<->"                               {\p s -> Assign p}

-- Bloque
"{"                                 {\p s -> CurlyBracketOpen p}
"}"                                 {\p s -> CurlyBracketClose p}
"|"                                 {\p s -> Bar p}

-- IO
"("                                 {\p s -> ParenthesisOpen p}
")"                                 {\p s -> ParenthesisClose p}
"@"                                 {\p s -> Record p}
"|>"                                {\p s -> PlaySym p}

-- Condicionales
if                                  {\p s -> If p}
else                                {\p s -> Else p}

-- Ciclos
loop                                {\p s -> Loop p}
":"                                 {\p s -> Colon p}
in                                  {\p s -> In p}
","                                 {\p s -> Comma p}
-- Stop y Next
">>"                                {\p s -> Next p}
"|]"                                {\p s -> Stop p}

-- Bemoles y Sostenidos
"#"                                 {\p s -> Sharp p}
"&"                                 {\p s -> Flat p}

-- Tracks
track                               {\p s -> Track p}
"||"                                {\p s -> DoubleBar p}
play                                {\p s -> Play p}
with                                {\p s -> With p}

-- New y Free
new                                 {\p s -> New p}
free                                {\p s -> Free p}

-- Silencios
\-\-.*                                    {\p _ -> RestToken { rest=HalfRestToken, posn=p } }
\~.*                                      {\p _ -> RestToken { rest=QuarterRestToken, posn=p } }
\*\/(\/(?!\*)|[^\/])*\/\*                 {\p _ -> RestToken { rest=EightRestToken, posn=p } }
\*{2}\/(\/(?!\*)|[^\/])*\/\*{2}           {\p _ -> RestToken { rest=16thRestToken, posn=p } }
\*{3}\/(\/(?!\*)|[^\/])*\/\*{3}           {\p _ -> RestToken { rest=32thRestToken, posn=p } }
\*{4}\/(\/(?!\*)|[^\/])*\/\*{4}           {\p _ -> RestToken { rest=64thRestToken, posn=p } }


-- Chords y Legatos
chord                               {\p s -> Chord p}
legato                              {\p s -> Legato p}

-- Operadores
--logicos
not                                 {\p s -> Not p}
and                                 {\p s -> And p}
or                                  {\p s -> Or p}

-- aritmeticos
"-"                                 {\p s -> Minus p}
mod                                 {\p s -> Mod p}
"/"                                 {\p s -> Div p}
"*"                                 {\p s -> Mult p}
"**"                                {\p s -> Pow p}
"+"                                 {\p s -> Plus p}

-- comparacion
"="                                 {\p s -> Equal p}
"/="                                {\p s -> NotEqual p}
"<"                                 {\p s -> Less p}
">"                                 {\p s -> Greater p}
"<="                                {\p s -> LessEqual p}
">="                                {\p s -> GreaterEqual p}

-- indexacion
"["                                 {\p s -> BracketOpen p}
"]"                                 {\p s -> BracketClose p}

-- Acceso a Chords
"."                                 {\p s -> Dot p}

-- Literales
$digits+                            {\p s -> Int p read(s) }
$digits+\.$digits+                  {\p s -> Float p read(s) }
@string                             {\p s -> String p s }
maj                                 {\p s -> Maj p}
min                                 {\p s -> Min p}


-- ID
[a-zA-Z]([a-zA-Z][0-9]\_)*\'*       {\p s -> Id p s}

{
    data Rest =
        HalfRestToken|
        QuarterRestToken|
        EightRestToken |
        16thRestToken |
        32thRestToken |
        64thRestToken
        deriving (Eq, Show)

    data Token = 
        -- Tipos de datos
        WholeToken { posn :: AlexPosn } |
        HalfToken { posn :: AlexPosn } |
        QuarterToken { posn :: AlexPosn } |
        EightToken { posn :: AlexPosn } |
        32thToken { posn :: AlexPosn } |
        64thToken { posn :: AlexPosn } |
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




