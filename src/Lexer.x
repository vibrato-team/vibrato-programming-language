{
module Lexer (Token(..), Rest(..), AlexPosn(..), lexAnalysis) where 
import Data.Either
}

%wrapper "posn"

-- Character sets and regular expressions
$digits = 0-9
$alpha = [a-zA-Z]

@string     = \"($printable # \")*\"              -- "strings
@char     = \'($printable # \")+\'                -- "chars

@commentcontent = (\/[^\*]|[^\/]|\n)*
@Eighththrests = (\*\/@commentcontent\/\*)
@Sixteenthrests = (\*\*\/@commentcontent\/\*\*)
@ThirtySecondrests = (\*\*\*\/@commentcontent\/\*\*\*)
@SixtyFourthrests = (\*\*\*\*\/@commentcontent\/\*\*\*\*)


tokens :-

    $white+                             ;

    -- Silencios
    \-\-.*                                    {\p s -> Right $ RestToken { token=s, rest=HalfRestToken, line=(posnLine p), col=(posnCol p) } }
    \~.*                                      {\p s -> Right $ RestToken { token=s, rest=QuarterRestToken, line=(posnLine p), col=(posnCol p) } }
    @Eighththrests                            {\p s -> Right $ RestToken { token=s, rest=EightRestToken, line=(posnLine p), col=(posnCol p) } }
    @Sixteenthrests                           {\p s -> Right $ RestToken { token=s, rest=SixteenthRestToken, line=(posnLine p), col=(posnCol p) } }
    @ThirtySecondrests                        {\p s -> Right $ RestToken { token=s, rest=ThirtySecondRestToken, line=(posnLine p), col=(posnCol p) } }
    @SixtyFourthrests                         {\p s -> Right $ RestToken { token=s, rest=SixtyFourthRestToken, line=(posnLine p), col=(posnCol p) } }

    -- Tipos de datos. 
    whole                               {\p s -> Right $ WholeToken s (posnLine p) (posnCol p)}
    half                                {\p s -> Right $ HalfToken s (posnLine p) (posnCol p)}
    quarter                             {\p s -> Right $ QuarterToken s (posnLine p) (posnCol p)}
    eight                               {\p s -> Right $ EightToken s (posnLine p) (posnCol p)}
    ThirtySecond                        {\p s -> Right $ ThirtySecondToken s (posnLine p) (posnCol p)}
    SixtyFourth                         {\p s -> Right $ SixtyFourthToken s (posnLine p) (posnCol p)}
    Melody                              {\p s -> Right $ MelodyToken s (posnLine p) (posnCol p)}
    Sample                              {\p s -> Right $ SampleToken s (posnLine p) (posnCol p)}

    -- Instrucciones

    -- Asignacion
    "<->"                               {\p s -> Right $ AssignToken s (posnLine p) (posnCol p)}

    -- Bloque
    "{"                                 {\p s -> Right $ CurlyBracketOpenToken s (posnLine p) (posnCol p)}
    "}"                                 {\p s -> Right $ CurlyBracketCloseToken s (posnLine p) (posnCol p)}
    "|"                                 {\p s -> Right $ BarToken s (posnLine p) (posnCol p)}

    -- IO
    "("                                 {\p s -> Right $ ParenthesisOpenToken s (posnLine p) (posnCol p)}
    ")"                                 {\p s -> Right $ ParenthesisCloseToken s (posnLine p) (posnCol p)}
    "@"                                 {\p s -> Right $ RecordToken s (posnLine p) (posnCol p)}
    "|>"                                {\p s -> Right $ PlaySymToken s (posnLine p) (posnCol p)}

    -- Condicionales
    if                                  {\p s -> Right $ IfToken s (posnLine p) (posnCol p)}
    else                                {\p s -> Right $ ElseToken s (posnLine p) (posnCol p)}

    -- Ciclos
    loop                                {\p s -> Right $ LoopToken s (posnLine p) (posnCol p)}
    ":"                                 {\p s -> Right $ ColonToken s (posnLine p) (posnCol p)}
    in                                  {\p s -> Right $ InToken s (posnLine p) (posnCol p)}
    ","                                 {\p s -> Right $ CommaToken s (posnLine p) (posnCol p)}
    -- Stop y Next
    ">>"                                {\p s -> Right $ NextToken s (posnLine p) (posnCol p)}
    "|]"                                {\p s -> Right $ StopToken s (posnLine p) (posnCol p)}

    -- Bemoles y Sostenidos
    "#"                                 {\p s -> Right $ SharpToken s (posnLine p) (posnCol p)}
    "&"                                 {\p s -> Right $ FlatToken s (posnLine p) (posnCol p)}

    -- Tracks
    track                               {\p s -> Right $ TrackToken s (posnLine p) (posnCol p)}
    "||"                                {\p s -> Right $ DoubleBarToken s (posnLine p) (posnCol p)}
    play                                {\p s -> Right $ PlayToken s (posnLine p) (posnCol p)}
    with                                {\p s -> Right $ WithToken s (posnLine p) (posnCol p)}

    -- New y Free
    new                                 {\p s -> Right $ NewToken s (posnLine p) (posnCol p)}
    free                                {\p s -> Right $ FreeToken s (posnLine p) (posnCol p)}

    -- Chords y Legatos
    chord                               {\p s -> Right $ ChordToken s (posnLine p) (posnCol p)}
    legato                              {\p s -> Right $ LegatoToken s (posnLine p) (posnCol p)}

    -- Operadores
    -- dereferencia
    \!                                  {\p s -> Right $ DereferenceToken s (posnLine p) (posnCol p)}

    --logicos
    not                                 {\p s -> Right $ NotToken s (posnLine p) (posnCol p)}
    and                                 {\p s -> Right $ AndToken s (posnLine p) (posnCol p)}
    or                                  {\p s -> Right $ OrToken s (posnLine p) (posnCol p)}

    -- aritmeticos
    "-"                                 {\p s -> Right $ MinusToken s (posnLine p) (posnCol p)}
    mod                                 {\p s -> Right $ ModToken s (posnLine p) (posnCol p)}
    "/"                                 {\p s -> Right $ DivToken s (posnLine p) (posnCol p)}
    "*"                                 {\p s -> Right $ MultToken s (posnLine p) (posnCol p)}
    "^"                                 {\p s -> Right $ PowToken s (posnLine p) (posnCol p)}
    "+"                                 {\p s -> Right $ PlusToken s (posnLine p) (posnCol p)}

    -- comparacion
    "="                                 {\p s -> Right $ EqualToken s (posnLine p) (posnCol p)}
    "/="                                {\p s -> Right $ NotEqualToken s (posnLine p) (posnCol p)}
    "<"                                 {\p s -> Right $ LessToken s (posnLine p) (posnCol p)}
    ">"                                 {\p s -> Right $ GreaterToken s (posnLine p) (posnCol p)}
    "<="                                {\p s -> Right $ LessEqualToken s (posnLine p) (posnCol p)}
    ">="                                {\p s -> Right $ GreaterEqualToken s (posnLine p) (posnCol p)}

    -- indexacion
    "["                                 {\p s -> Right $ BracketOpenToken s (posnLine p) (posnCol p)}
    "]"                                 {\p s -> Right $ BracketCloseToken s (posnLine p) (posnCol p)}

    -- Acceso a Chords
    "."                                 {\p s -> Right $ DotToken s (posnLine p) (posnCol p)}

    -- Literales
    $digits+                            {\p s -> Right $ IntToken s (posnLine p) (posnCol p) (read s) }
    $digits+\.$digits+                  {\p s -> Right $ FloatToken s (posnLine p) (posnCol p) (read s) }
    @string                             {\p s -> Right $ StringToken s (posnLine p) (posnCol p) (read s) }
    @char                               {\p s -> Right $ CharToken s (posnLine p) (posnCol p) (read s)}
    maj                                 {\p s -> Right $ MajToken s (posnLine p) (posnCol p)}
    min                                 {\p s -> Right $ MinToken s (posnLine p) (posnCol p)}

    -- ID
    [a-zA-Z][a-zA-Z0-9\_]*\'*             {\p s -> Right $ IdToken s (posnLine p) (posnCol p)}

    "=="                                {\p _ -> didYouMeanToken p "\"=\""}
    "!="                                {\p _ -> didYouMeanToken p "\"/=\""}
    "<-"                                {\p _ -> didYouMeanToken p "\"<->\""}
    .                                   {\p s -> Left $ "Lexical error at line " ++ show (posnLine p) ++ " and column " ++ show (posnCol p) ++ ": " ++ show s}

{

-- This function is basically the lexer
lexAnalysis :: String -> Either [String] [Token]
lexAnalysis source = 
    if errors /= [] then Left errors
    else Right matches
    where
        eitherTokens = alexScanTokens source
        errors = lefts eitherTokens
        matches = rights eitherTokens

-- Helper functions
didYouMeanToken :: AlexPosn -> String -> Either String Token
didYouMeanToken p op = Left $ "Did you mean " ++ op ++ " at line: " ++ show (posnLine p) ++ ", col: " ++ show (posnCol p) ++ "?"

posnLine :: AlexPosn -> Int
posnLine (AlexPn _ l _) = l

posnCol :: AlexPosn -> Int
posnCol (AlexPn _ _ c) = c

-- Data types
-- rests
data Rest =
    HalfRestToken|
    QuarterRestToken|
    EightRestToken |
    SixteenthRestToken |
    ThirtySecondRestToken |
    SixtyFourthRestToken
    deriving (Eq, Show)

-- tokens
data Token = 
    -- Tipos de datos
    WholeToken { token :: String, line :: Int, col :: Int } |
    HalfToken { token :: String, line :: Int, col :: Int } |
    QuarterToken { token :: String, line :: Int, col :: Int } |
    EightToken { token :: String, line :: Int, col :: Int } |
    ThirtySecondToken { token :: String, line :: Int, col :: Int } |
    SixtyFourthToken { token :: String, line :: Int, col :: Int } |
    MelodyToken { token :: String, line :: Int, col :: Int } |
    SampleToken { token :: String, line :: Int, col :: Int } |
    
    -- Instrucciones
    AssignToken { token :: String, line :: Int, col :: Int } |
    CurlyBracketOpenToken { token :: String, line :: Int, col :: Int } |
    CurlyBracketCloseToken { token :: String, line :: Int, col :: Int } |
    BarToken { token :: String, line :: Int, col :: Int } |
    ParenthesisOpenToken { token :: String, line :: Int, col :: Int } |
    ParenthesisCloseToken { token :: String, line :: Int, col :: Int } |
    RecordToken { token :: String, line :: Int, col :: Int } |
    PlaySymToken { token :: String, line :: Int, col :: Int } |
    IfToken { token :: String, line :: Int, col :: Int } |
    ElseToken { token :: String, line :: Int, col :: Int } |
    LoopToken { token :: String, line :: Int, col :: Int } |
    ColonToken { token :: String, line :: Int, col :: Int } |
    InToken { token :: String, line :: Int, col :: Int } |
    CommaToken { token :: String, line :: Int, col :: Int } |
    NextToken { token :: String, line :: Int, col :: Int } |
    StopToken { token :: String, line :: Int, col :: Int } |
    SharpToken { token :: String, line :: Int, col :: Int } |
    FlatToken { token :: String, line :: Int, col :: Int } |
    TrackToken { token :: String, line :: Int, col :: Int } |
    DoubleBarToken { token :: String, line :: Int, col :: Int } |
    PlayToken { token :: String, line :: Int, col :: Int } |
    WithToken { token :: String, line :: Int, col :: Int } |
    NewToken { token :: String, line :: Int, col :: Int } |
    FreeToken { token :: String, line :: Int, col :: Int } |
    RestToken { token :: String, line :: Int, col :: Int, rest :: Rest } |
    ChordToken { token :: String, line :: Int, col :: Int } |
    LegatoToken { token :: String, line :: Int, col :: Int } |

    -- Operadores
    DereferenceToken { token :: String, line :: Int, col :: Int } |
    NotToken { token :: String, line :: Int, col :: Int } |
    AndToken { token :: String, line :: Int, col :: Int } |
    OrToken { token :: String, line :: Int, col :: Int } |
    MinusToken { token :: String, line :: Int, col :: Int } |
    ModToken { token :: String, line :: Int, col :: Int } |
    DivToken { token :: String, line :: Int, col :: Int } |
    MultToken { token :: String, line :: Int, col :: Int } |
    PowToken { token :: String, line :: Int, col :: Int } |
    PlusToken { token :: String, line :: Int, col :: Int } |
    EqualToken { token :: String, line :: Int, col :: Int } |
    NotEqualToken { token :: String, line :: Int, col :: Int } |
    LessToken { token :: String, line :: Int, col :: Int } |
    GreaterToken { token :: String, line :: Int, col :: Int } |
    LessEqualToken { token :: String, line :: Int, col :: Int } |
    GreaterEqualToken { token :: String, line :: Int, col :: Int } |
    BracketOpenToken { token :: String, line :: Int, col :: Int } |
    BracketCloseToken { token :: String, line :: Int, col :: Int } |
    DotToken { token :: String, line :: Int, col :: Int } |

    -- Identificador
    IdToken { token :: String, line :: Int, col :: Int} |

    -- Literales
    IntToken { token :: String, line :: Int, col :: Int, intLiteral :: Int} |
    FloatToken { token :: String, line :: Int, col :: Int, floatLiteral :: Float} |
    MajToken { token :: String, line :: Int, col :: Int } |
    MinToken { token :: String, line :: Int, col :: Int } |
    StringToken { token :: String, line :: Int, col :: Int, stringLiteral :: String} |
    CharToken { token :: String, line :: Int, col :: Int, charLiteral :: Char}

    deriving (Eq, Show)

}