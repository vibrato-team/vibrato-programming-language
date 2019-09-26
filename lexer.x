{
    module Lexer (Token(..), AlexPosn(..), alexScanTokens, token_posn) where 
}

%wrapper "posn"

tokens :-

$white+         ;

-- Tipos de datos. 
whole           {/p s -> Whole p} -- :: AlexPosn -> String -> Token
half            {/p s -> Half p}
quarter         {/p s -> Quarter p}
eight           {/p s -> Eight p}
32th            {/p s -> 32th p}
64th            {/p s -> 64th p}
Melody          {/p s -> Melody p}
Sample          {/p s -> Sample p}

-- Instrucciones
"<->" 
"{"
"{"
"|"
"("
")"
"@"
"|>"
if
else
loop


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
}




