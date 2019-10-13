{
module Lexer (Error(..), Token(..), Alex, alexError, runAlex, runAlexScan, alexEOF, lexerWrapper, AlexUserState(..), AlexState(..)) where
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

    -- Silencios TODO para el MIDI
    \-\-.*                              ;
    \~.*                                ;
    @Eighththrests                      ;
    @Sixteenthrests                     ;
    @ThirtySecondrests                  ;
    @SixtyFourthrests                   ;

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
    "{"                                 { pushToken OpenCurlyToken }
    "}"                                 { pushToken CloseCurlyToken }
    "|"                                 { pushToken BarToken }

    -- IO
    "("                                 { pushToken OpenParToken }
    ")"                                 { pushToken CloseParToken }
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
    moderato                            { pushToken MainToken }

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
    [a-z][a-zA-Z0-9\_]*\'*              { pushToken IdToken }
    [A-Z][a-zA-Z0-9\_]*\'*              { pushToken IdTypeToken }

    "=="                                { throwUserError }
    "!="                                { throwUserError }
    "<-"                                { throwUserError }
    .                                   { throwUserError }                                 

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

throwError :: AlexUserState -> Alex AlexUserState
throwError ust = case matches ust of
    Left errors -> error $ concatMap (++ "\n") $ map show errors
    Right _ -> return ust

-- Lexer wrapper for monadic parser
lexerWrapper :: (Token -> Alex a) -> Alex a
lexerWrapper cont = do
    token <- alexMonadScan
    cont token
--- Run scanner
runAlexScan :: String -> Either String AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState
--------------------------------------------------------
--------------------------------------------------------

-- Push token to the monad
type TokenConstructor = String -> Int -> Int -> Token

pushToken :: TokenConstructor -> AlexAction Token
pushToken constructor (p, _, _, s) len = modifyUserState (pushTokenToState tk) >> alexMonadScan where
    tokenString = take len s
    tk = constructor tokenString (posnLine p) (posnCol p)

throwUserError :: AlexAction Token
throwUserError (p, _, _, str) len = (Alex $ \s -> Right (s{ alex_ust= pushError s }, ())) >> alexMonadScan where
    tokenString = take len str
    newError = Error (posnLine p) (posnCol p) tokenString
    pushError s = AlexUserState $ Left $ newError : (fromLeft [] (matches $ alex_ust s))

-- Lexical error
data Error = Error { errLine :: Int, errCol :: Int, errorToken :: String }
instance Show Error where
    show err = "Invalid token" ++ suggestion where
            suggestion = case (errorToken err) of
                "==" -> ". Did you mean \"=\"?"
                "!=" -> ". Did you mean \"/=\"?"
                "<-" -> ". Did you mean \"<->\"?"
                _ -> ":"

}