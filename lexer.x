{
    module Lexer (Token(..), AlexPosn(..), alexScanTokens, token_posn) where 
}

%wrapper "posn"

-- Character sets used in the regular expressions
$digit = 0-9
$alpha = [a-zA-Z] 

tokens :-

$white+         ;

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
"|"                                 {\p s -> Pipe p}

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

-- Comentarios
[(\-\-)\~].*                        {\p s -> Comment p}

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

-- Prelude
to_ascii                            {\p s -> ToAscii p}
from_ascii                          {\p s -> FromAscii p}
length                              {\p s -> Length p}
"<|>"                               {\p s -> Concat p}

-- Literales
$digits+                            {\p s -> Int p}
$digits+\.$digits+                  {\p s -> Float p}
Maj                                 {\p s -> True p}
Min                                 {\p s -> False p}


-- ID
[a-zA-Z]([a-zA-Z][0-9]\_)*\'*       {\p s -> Id p}

-- ID Acordes
[A-Z]([a-zA-Z][0-9]\_)*\'*          {\p s -> IdChord p}

-- ID Track
[a-z]([a-zA-Z][0-9]\_)*\'*          {\p s -> IdTrack p}



{
    data Token = 
        Whole AlexPosn |
        Half AlexPosn |
        Quarter AlexPosn |
        Eight AlexPosn |
        32th AlexPosn |
        64th AlexPosn |
        Melody AlexPosn |
        Sample AlexPosn |
        Assign AlexPosn |
        CurlyBracketOpen AlexPosn |
        CurlyBracketClose AlexPosn |
        Pipe AlexPosn |
        ParenthesisOpen AlexPosn |
        ParenthesisClose AlexPosn |
        Record AlexPosn |
        PlaySym AlexPosn |
        If AlexPosn |
        Else AlexPosn |
        Loop AlexPosn |
        Colon AlexPosn |
        In AlexPosn |
        Comma AlexPosn |
        Next AlexPosn |
        Stop AlexPosn |
        Sharp AlexPosn |
        Flat AlexPosn |
        Track AlexPosn |
        DoubleBar AlexPosn |
        Play AlexPosn |
        With AlexPosn |
        New AlexPosn |
        Free AlexPosn |
        Chord AlexPosn |
        Legato AlexPosn |
        Not AlexPosn |
        And AlexPosn |
        Or AlexPosn |
        Minus AlexPosn |
        Mod AlexPosn |
        Div AlexPosn |
        Mult AlexPosn |
        Pow AlexPosn |
        Plus AlexPosn |
        Equal AlexPosn |
        NotEqual AlexPosn |
        Less AlexPosn |
        Greater AlexPosn |
        LessEqual AlexPosn |
        GreaterEqual AlexPosn |
        BracketOpen AlexPosn |
        BracketClose AlexPosn |
        Dot AlexPosn |
        ToAscii AlexPosn |
        FromAscii AlexPosn |
        Length AlexPosn |
        Concat AlexPosn |
        Id AlexPosn String |
        IdChord AlexPosn String |
        IdTrack AlexPosn String |
        Int AlexPosn Int |
        Float AlexPosn Float |
        True AlexPosn |
        False AlexPosn |
        String AlexPosn String |
        Char AlexPosn Char |
        
}




