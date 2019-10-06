{
module Lexer (Token(..), runAlexScan, AlexUserState(..), AlexState(..)) where
import Tokens
import Data.Either
}

%wrapper "monadUserState"

-- Character sets and regular expressions
$digits = 0-9
$alpha = [a-zA-Z]
@escchars = \\[nt\"\'\\]                                  -- "escaped characters

@string     = \"([$printable # \"] | @escchars)*\"              -- "strings
@char     = \'([$printable # \'] | @escchars)+\'                

@commentcontent = (\/[^\*]|[^\/]|\n)*
@Eighththrests = (\*\/@commentcontent\/\*)
@Sixteenthrests = (\*\*\/@commentcontent\/\*\*)
@ThirtySecondrests = (\*\*\*\/@commentcontent\/\*\*\*)
@SixtyFourthrests = (\*\*\*\*\/@commentcontent\/\*\*\*\*)

tokens :-

    $white+                             ;

    -- Silencios
    \-\-.*                              { pushToken $ RestToken HalfRestToken }
    \~.*                                { pushToken $ RestToken QuarterRestToken }
    @Eighththrests                      { pushToken $ RestToken EightRestToken }
    @Sixteenthrests                     { pushToken $ RestToken SixteenthRestToken }
    @ThirtySecondrests                  { pushToken $ RestToken ThirtySecondRestToken }
    @SixtyFourthrests                   { pushToken $ RestToken SixtyFourthRestToken }

    -- Tipos de datos. 
    whole                               { pushToken WholeToken }
    half                                { pushToken HalfToken }
    quarter                             { pushToken QuarterToken }
    eight                               { pushToken EightToken }
    ThirtySecond                        { pushToken ThirtySecondToken }
    SixtyFourth                         { pushToken SixtyFourthToken }
    Melody                              { pushToken MelodyToken }
    Sample                              { pushToken SampleToken }

    -- Instrucciones

    -- Asignacion
    "<->"                               { pushToken AssignToken }

    -- Bloque
    "{"                                 { pushToken CurlyBracketOpenToken }
    "}"                                 { pushToken CurlyBracketCloseToken }
    "|"                                 { pushToken BarToken }

    -- IO
    "("                                 { pushToken ParenthesisOpenToken }
    ")"                                 { pushToken ParenthesisCloseToken }
    "@"                                 { pushToken RecordToken }
    "|>"                                { pushToken PlaySymToken }

    -- Condicionales
    if                                  { pushToken IfToken }
    else                                { pushToken ElseToken }

    -- Ciclos
    loop                                { pushToken LoopToken }
    ":"                                 { pushToken ColonToken }
    in                                  { pushToken InToken }
    ","                                 { pushToken CommaToken }
    
    -- Stop y Next
    ">>"                                { pushToken NextToken }
    "|]"                                { pushToken StopToken }

    -- Bemoles y Sostenidos
    "#"                                 { pushToken SharpToken }
    "&"                                 { pushToken FlatToken }

    -- Tracks
    track                               { pushToken TrackToken }
    "||"                                { pushToken DoubleBarToken }
    play                                { pushToken PlayToken }
    with                                { pushToken WithToken }

    -- New y Free
    new                                 { pushToken NewToken }
    free                                { pushToken FreeToken }

    -- Chords y Legatos
    chord                               { pushToken ChordToken }
    legato                              { pushToken LegatoToken }

    -- Operadores
    -- dereferencia
    \!                                  { pushToken DereferenceToken }

    --logicos
    not                                 { pushToken NotToken }
    and                                 { pushToken AndToken }
    or                                  { pushToken OrToken }

    -- aritmeticos
    "-"                                 { pushToken MinusToken }
    mod                                 { pushToken ModToken }
    "/"                                 { pushToken DivToken }
    "*"                                 { pushToken MultToken }
    "^"                                 { pushToken PowToken }
    "+"                                 { pushToken PlusToken }

    -- comparacion
    "="                                 { pushToken EqualToken }
    "/="                                { pushToken NotEqualToken }
    "<"                                 { pushToken LessToken }
    ">"                                 { pushToken GreaterToken }
    "<="                                { pushToken LessEqualToken }
    ">="                                { pushToken GreaterEqualToken }

    -- indexacion
    "["                                 { pushToken BracketOpenToken }
    "]"                                 { pushToken BracketCloseToken }

    -- Acceso a Chords
    "."                                 { pushToken DotToken }

    -- Literales
    $digits+                            { pushToken IntToken  }
    $digits+\.$digits+                  { pushToken FloatToken }
    @string                             { pushToken StringToken }
    @char                               { pushToken CharToken }
    maj                                 { pushToken MajToken }
    min                                 { pushToken MinToken }

    -- ID
    [a-zA-Z][a-zA-Z0-9\_]*\'*           { pushToken IdToken }

    "=="                                { throwError }
    "!="                                { throwError }
    "<-"                                { throwError }
    .                                   { throwError }                                 

{

alexEOF :: Alex Token
alexEOF = return EOFToken

--------------------------------------------------------
--------------------------------------------------------

-- Helper functions
posnLine :: AlexPosn -> Int
posnLine (AlexPn _ l _) = l

posnCol :: AlexPosn -> Int
posnCol (AlexPn _ _ c) = c

    --------------------------------------------------------
--------------------------------------------------------

-- State of the monad
data AlexUserState = AlexUserState {
    matches :: Either [Error] [Token]
}

-- Modify user state
modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s -> Right (s{ alex_ust=(f $ alex_ust s) }, ())

-- push token to state
pushTokenToState :: Token -> AlexUserState -> AlexUserState
pushTokenToState tk state = state { matches=newUserState } where
    newUserState = case matches state of
        ust@(Left _) -> ust
        Right tks -> Right $ tks ++ [tk]

-- Initial state
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
    matches=Right []
}

-- get state from Alex a
getUserState :: Alex AlexUserState
getUserState = Alex $ \s -> Right (s, alex_ust s)

-- Run scanner
runAlexScan :: String -> Either String AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState

--------------------------------------------------------
--------------------------------------------------------

-- Push token to the monad
type StdTokenConstructor = String -> Int -> Int -> Token

pushToken :: StdTokenConstructor -> AlexAction ()
pushToken constructor (p, _, _, s) len = modifyUserState (pushTokenToState tk) >> alexMonadScan where
    tokenString = take len s
    tk = constructor tokenString (posnLine p) (posnCol p)

throwError :: AlexAction ()
throwError (p, _, _, str) len = (Alex $ \s -> Right (s{ alex_ust= pushError s }, ())) >> alexMonadScan where
    newError = Error p (take len str )
    pushError s = AlexUserState $ Left $ fromLeft [] (matches $ alex_ust s) ++ [newError]

-- Lexical error
data Error = Error { posn :: AlexPosn, errorToken :: String }
instance Show Error where
    show err = "Invalid token at line " ++ show (posnLine (posn err) )++
        ", column " ++ show (posnCol (posn err)) ++ ": " ++ show (errorToken err) ++
            ". " ++ suggestion where
            suggestion = case (errorToken err) of
                "==" -> "Did you mean \"=\"?"
                "!=" -> "Did you mean \"/=\""
                "<-" -> "Did you mean \"<->\""
                _ -> ""

}