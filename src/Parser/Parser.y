{
module Parser.Parser(parse, createFunctionEntry, createParamEntry, createTypeEntry, parseError, PMonad.ParserMonad) where
import Lexer
import Tokens
import qualified AST
import Util.Error
import Data.Either
import Data.Maybe
import Parser.Monad (ParserMonad)
import qualified Parser.Monad as PMonad
import qualified Control.Monad.RWS.Lazy as RWS
import Control.Monad.Trans
import qualified Semantic.Data as SemData 
import Semantic.Analyzers
import Data.List
}

%name parse
%error { parseError }
%tokentype { Token }
%monad { PMonad.ParserMonad }

%token
    whole               { WholeToken _ _ _ }
    half                { HalfToken _ _ _ }
    quarter             { QuarterToken _ _ _ }
    eighth              { EightToken _ _ _ }
    melody              { MelodyToken _ _ _ }
    sample              { SampleToken _ _ _ }
    ThirtySecond        { ThirtySecondToken _ _ _ }
    SixtyFourth         { SixtyFourthToken _ _ _ }

    TT                  { TTToken _ _ _ }

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
    id_type             { IdTypeToken _ _ _ }

    main                { MainToken _ _ _ }

%nonassoc play
%nonassoc loop
%nonassoc '<->'
%nonassoc '=' '/=' 
%nonassoc '>' '<' '<=' '>='
%left LVALUE
%left '+' '-'
%left '*' '/' mod
%left '^'
%left NEG '#' '&'
%left and or
%right not
%right '['
%left ']'
%left '!'
%left '.'

%nonassoc error

%%

Start                   :: { () }
Start                   : ExternalList MainDeclaration      { }

ExternalList            :: { () }
ExternalList            : ExternalList FunctionDeclaration  { }             
                        | ExternalList ChordLegato          { }
                        | {- empty -}                       { }

-- TODO: Agregar los params y los fields en un nuevo scope
FunctionDeclaration     :: { () }                                           -- add block to function entry
FunctionDeclaration     : Signature Block PopScope                          {% do
                                                                                PMonad.updateEntry (addBlock $2) $1
                                                                                PMonad.clearReturnType }
MainDeclaration         : main PushScope '(' ClosePar Block                 {% do
                                                                                pushIfError $4 $3 $ matchingError "parentheses"
                                                                                let idMain = AST.Id $1 in createFunctionEntry (AST.id_token idMain) Nothing idMain [] (Just $5) }

Signature               :: { String }
Signature               : track Id PushScope '(' ListaParam ClosePar MaybeType             {% do
                                                                                                pushIfError $6 $4 $ matchingError "parentheses"
                                                                                                let tk = AST.id_token $2
                                                                                                PMonad.addReturnType $7
                                                                                                return $ token tk }

ListaParam              :: { [AST.VarDeclaration] }
ListaParam              : ParamDeclaration                          { [$1] }
                        | ListaParam ',' ParamDeclaration           { $3 : $1 }
                        | {- empty -}                               { [] }

ParamDeclaration        :: { AST.VarDeclaration }
ParamDeclaration        : Id ':' ParamRef Type                      { AST.VarDec $1 $4 }
ParamRef                :: { Bool }
ParamRef                : '>'                                       { True }
                        | {- empty -}                               { False }

Id                      :: { AST.Id }
Id                      : id                                    { AST.Id $1 }
                        -- Boolean literals
                        | maj                                   { AST.Id $1}
                        | min                                   { AST.Id $1 }
                        -- Null expression
                        | TT                                    { AST.Id $1 }

--------------------------------------------------------------------------------
--------------------------For error recovery------------------------------------
--------------------------------------------------------------------------------

MaybeType               :: { Maybe AST.Type }
MaybeType               : {- empty -}                           { Nothing }
                        | ':' Type                              { Just $2 }

CloseBracket             : '}'          { True }
                        | error         { False }

ClosePar                : ')'          { True }
                        | error         { False }

CloseAngular            : '>'       { True }
                        | error     { False }

CloseSquare             : ']'       { True }
                        | error     { False }

With                    : with      { True }
                        | error     { False }


In                      : in        { True }
                        | error     { False }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


Block                   :: { AST.Block }
Block                   : PushScope '{' Seq CloseBracket PopScope                  {%do
                                                                                        pushIfError $4 $2 $ matchingError "bracket"
                                                                                        return $ AST.Block $ reverse $ filter notDeclaration $3 }

Seq                     :: { [AST.Instruction] }
Seq                     : Instruction                           { [$1] }
                        | Seq Instruction                       { $2 : $1 }

Instruction             :: { AST.Instruction }
Instruction             : OpenCondition                         { $1 }
                        | ClosedCondition                       { $1 }

OpenCondition           :: { AST.Instruction }
OpenCondition           : if '(' Expression ClosePar Instruction                         {%do
                                                                                            pushIfError $4 $2 $ matchingError "parentheses"
                                                                                            checkExpType $3 [AST.Simple "whole"] $2
                                                                                            return $ AST.IfInst $3 $5 Nothing }
                        | if '(' Expression ClosePar ClosedCondition else OpenCondition  {%do
                                                                                            pushIfError $4 $2 $ matchingError "parentheses"
                                                                                            checkExpType $3 [AST.Simple "whole"] $2
                                                                                            return $ AST.IfInst $3 $5 (Just $7) }

ClosedCondition         :: { AST.Instruction }
ClosedCondition         : if '(' Expression ClosePar ClosedCondition else ClosedCondition    {%do
                                                                                                pushIfError $4 $2 $ matchingError "parentheses"
                                                                                                checkExpType $3 [AST.Simple "whole"] $2
                                                                                                return $ AST.IfInst $3 $5 (Just $7) }
                        | SimpleInstruction                                             { $1 }

SimpleInstruction       :: { AST.Instruction }
SimpleInstruction       : Block                 { AST.BlockInst $1 }
                        | Loop                                  { $1 }
                        | '>>'                                  { AST.NextInst }
                        | '|]'                                  { AST.StopInst }
                        | Return                                { $1 }
                        | Statement '|'                         { $1 }

Statement               :: { AST.Instruction }
Statement               : VarDeclaration                        { AST.VarDecInst $1 }
                        | VarInit                               { $1 }
                        | Asignacion                            { $1 }
                        | IO                                    { $1 }
                        | free Id                               {% do
                                                                    checkVarIsDeclared $ AST.id_token $2
                                                                    return (AST.FreeInst $2) }
                        | LValue '#'                            {% do 
                                                                    checkConstLvalue $1 
                                                                    return $ AST.SharpExp $1 }
                        | LValue '&'                            {% do 
                                                                    checkConstLvalue $1 
                                                                    return $ AST.FlatExp $1 }
                        | CallFuncion                           { AST.CallFuncInst $1 }


VarDeclaration          :: { AST.VarDeclaration }
VarDeclaration          : Id ':' Type                           {% do
                                                                    createVarEntry (AST.id_token $1) (Just $3)
                                                                    return $ AST.VarDec $1 $3}

VarInit                 :: { AST.Instruction }
VarInit                 : Id ':' Type '<->' Expression          {% do 
                                                                    createVarEntry (AST.id_token $1) (Just $3)
                                                                    let idExp = AST.IdExp $1 $3
                                                                        expType = AST.exp_type $5

                                                                    checkAssignment idExp $5 $4
                                                                    return $ AST.AssignInst idExp $ castExp $5 $3 }

Asignacion              :: { AST.Instruction }
Asignacion              : LValue '<->' Expression               {%do
                                                                    checkAssignment $1 $3 $2
                                                                    checkConstLvalue $1

                                                                    let lType = AST.exp_type $1
                                                                        expType = AST.exp_type $3
                                                                    return $ AST.AssignInst $1 $ castExp $3 lType }

LValue                  :: { AST.Expression }
LValue                  : Indexing                              { $1 }
                        | DotExpression                         { $1 }
                        | Dereference                           { $1 }
                        | Id                                    {% do
                                                                    checkVarIsDeclared (AST.id_token $1)
                                                                    idType <- PMonad.lookup $ token $ AST.id_token $1
                                                                    return $ (AST.IdExp $1 (fromJust $ SemData.entry_type $ fromJust idType)) }

Return                  :: { AST.Instruction }
Return                  : Expression '||'                       {% do
                                                                    state <- RWS.get
                                                                    case PMonad.state_ret_type state of
                                                                        Nothing -> do
                                                                            pushError $2 $ show (AST.exp_type $1) ++ " return instruction inside void track:"
                                                                            return $ AST.ReturnInst $ Just $1

                                                                        Just retType -> do
                                                                            checkAssignment' retType $1 $2
                                                                            return $ AST.ReturnInst $ Just $ castExp $1 retType }
                        | '||'                                  {% do
                                                                    state <- RWS.get
                                                                    case PMonad.state_ret_type state of
                                                                        Just retType -> pushError $1 $ "Void return instruction inside " ++ show retType ++ " track:"
                                                                        Nothing -> return ()
                                                                    return $ AST.ReturnInst Nothing}

IO                      :: { AST.Instruction }
IO                      : '@' '(' ListExp ClosePar                   {%do
                                                                        pushIfError $4 $2 $ matchingError "parentheses" 
                                                                        return $ AST.RecordInst $ reverse $3 }
                        | '|>' '(' ListExp ClosePar                  {%do
                                                                        pushIfError $4 $2 $ matchingError "parentheses" 
                                                                        return $ AST.PlayInst $ reverse $3 }

Loop                    :: { AST.Instruction }
Loop                    : loop PushScope IdConst Block PopScope In '(' Expression ClosePar                                       {%do
                                                                                                                                    -- Error recovery
                                                                                                                                    pushIfError $6 $1 "Missing \"in\" in loop instruction"
                                                                                                                                    pushIfError $9 $7 $ matchingError "parentheses"

                                                                                                                                    checkExpType $8 AST.numberTypes $7
                                                                                                                                    return $ AST.ForInst (fst $3) (snd $3) $4 Nothing $8 Nothing }
                        | loop PushScope IdConst Block PopScope In '(' Expression ',' Expression ClosePar                        {%do
                                                                                                                                    -- Error recovery
                                                                                                                                    pushIfError $6 $1 "Missing \"in\" in loop instruction"
                                                                                                                                    pushIfError $11 $7 $ matchingError "parentheses"

                                                                                                                                    checkExpType $8 AST.numberTypes $7
                                                                                                                                    checkExpType $10 AST.numberTypes $9
                                                                                                                                    return $ AST.ForInst (fst $3) (snd $3) $4 (Just $8) $10 Nothing }
                        | loop PushScope IdConst Block PopScope In '(' Expression ',' Expression ',' Expression ClosePar         {%do
                                                                                                                                    -- Error recovery
                                                                                                                                    pushIfError $6 $1 "Missing \"in\" in loop instruction"
                                                                                                                                    pushIfError $13 $7 $ matchingError "parentheses"
                                                                                                                                    
                                                                                                                                    checkExpType $8 AST.numberTypes $7
                                                                                                                                    checkExpType $10 AST.numberTypes $9
                                                                                                                                    checkExpType $12 AST.numberTypes $11
                                                                                                                                    return $ AST.ForInst (fst $3) (snd $3) $4 (Just $8) $10 (Just $12) }
                        | loop '(' Expression ClosePar Block     {% do
                                                                    pushIfError $4 $2 $ matchingError "parentheses"
                                                                    checkExpType $3 [AST.Simple "whole"] $2
                                                                    return $ AST.WhileInst $3 $5 }
IdConst                 :: { (AST.Id, Maybe AST.Type) }
IdConst                 : id MaybeType                                   {% do 
                                                                            createConstEntry $1 $2   
                                                                            return $ (AST.Id $1, $2) }


CallFuncion             :: { AST.Expression }
CallFuncion             : play Id With '(' ListExp ClosePar          {% do
                                                                        -- Error recovery
                                                                        pushIfError $3 $1 "Missing \"with\" in play instruction:"
                                                                        pushIfError $6 $4 $ matchingError "parentheses"

                                                                        entry <- checkVarIsDeclared (AST.id_token $2)
                                                                        let category = SemData.entry_category entry
                                                                        case category of
                                                                            SemData.Function _ params -> do
                                                                                -- Verificacion of the list of expressions
                                                                                casted_params <- checkParams $2 (reverse $5) params
                                                                                return $ AST.CallExp $2 casted_params (fromMaybe voidType $ SemData.entry_type entry)

                                                                            _ -> do
                                                                                pushError $1 "Calling a not track expression:"
                                                                                return $ AST.CallExp $2 (reverse $5) (fromMaybe voidType $ SemData.entry_type entry) }

                        | play Id                               {% do
                                                                            entry <- checkVarIsDeclared (AST.id_token $2)
                                                                            let category = SemData.entry_category entry
                                                                            case category of
                                                                                SemData.Function _ params ->
                                                                                    if length params == 0
                                                                                        then return () 
                                                                                        else pushError (AST.id_token $2) "Wrong number of arguments:"
                                                                                _ -> pushError $1 "Calling a not track expression:"
                                                                            return $ AST.CallExp $2 [] (fromMaybe voidType $ SemData.entry_type entry)}

ListExp                 :: { [AST.Expression] }
ListExp                 : Expression                            { [$1] }
                        | ListExp ',' Expression                { $3 : $1 }
                        | {-empty-}                             { [] }

Indexing                :: { AST.Expression }
Indexing                : Expression '[' Expression CloseSquare     {% do 
                                                                        -- Error recovery
                                                                        pushIfError $4 $2 $ matchingError "square bracket"

                                                                        -- Check left expression is a melody
                                                                        let expType = AST.exp_type $1
                                                                        if AST.type_str expType /= "Melody"
                                                                            then pushError $2 "Indexing a not melody expression:"
                                                                            else return ()

                                                                        -- Check index expression is an integer
                                                                        checkExpType $3 [AST.Simple "quarter", AST.Simple "eighth"] $2

                                                                        return $ AST.IndexingExp $1 $3 $2 (AST.type_type $ AST.exp_type $1) }

DotExpression           :: { AST.Expression }
DotExpression           : Expression '.' Id                         {% do
                                                                        let lType = AST.exp_type $1
                                                                        entryMaybe <- PMonad.lookup $ AST.type_str lType
                                                                        let entry = fromJust entryMaybe
                                                                        let levelMaybe = SemData.entry_level entry
                                                                        throwIfNothing levelMaybe $2 "Expression is not a chord or legato:"

                                                                        fieldEntry <- analyzeField (AST.id_token $3) (fromJust levelMaybe)
                                                                        return $ AST.DotExp $1 $3 (fromJust $ SemData.entry_type fieldEntry) }

Dereference             :: { AST.Expression }
Dereference             : Expression '!'                            {% do 
                                                                        let expType = AST.exp_type $1
                                                                        if AST.type_str expType /= "Sample"
                                                                            then pushError $2 "Dereferencing a not sample expression:"
                                                                            else return ()
                                                                        return $ AST.DereferenceExp $1 $2 (AST.type_type $ AST.exp_type $1) }

Type                    :: { AST.Type }
Type                    : whole                                 { AST.Simple (token $1) }
                        | half                                  { AST.Simple (token $1) }
                        | quarter                               { AST.Simple (token $1) }
                        | eighth                                { AST.Simple (token $1) }
                        | ThirtySecond                          { AST.Simple (token $1) }
                        | SixtyFourth                           { AST.Simple (token $1) }
                        | melody '<' Type CloseAngular          {%do
                                                                    pushIfError $4 $2 $ matchingError "angle bracket"
                                                                    return $ AST.Compound (token $1) $3 }
                        | sample '<' Type CloseAngular          {%do
                                                                    pushIfError $4 $2 $ matchingError "angle bracket"
                                                                    return $ AST.Compound (token $1) $3 }
                        | IdType                                { $1 }

Literal                 :: { AST.Expression }
Literal                 : int                                   { AST.Literal $1 (AST.Simple "quarter") }
                        | float                                 { AST.Literal $1 (AST.Simple "32th") }
                        | string                                { AST.Literal $1 (AST.Compound "Melody" $ AST.Simple "half") }
                        | char                                  { AST.Literal $1 (AST.Simple "half") }
                        | LiteralMelody                         { $1 }

                        | Type '(' ListExp ClosePar             {% do 
                                                                    pushIfError $4 $2 $ matchingError "parentheses"
                                                                    -- Check if listExp is equal type of fields
                                                                    entry <- PMonad.lookup (AST.type_str $1)
                                                                    case entry of
                                                                        Just a -> case (SemData.type_adt $ SemData.entry_category a) of
                                                                            Just SemData.Chord -> checkFieldsChord $2 (reverse $3) (SemData.type_fields $ SemData.entry_category a)
                                                                            Just SemData.Legato -> checkFieldsLegato $2 (reverse $3) (SemData.type_fields $ SemData.entry_category a)
                                                                            _ -> pushError $2 "Type are not Chord or Legato: "
                                                                        Nothing -> pushError $2 "Type not declare: "

                                                                    if $1 `elem` AST.primitiveTypes
                                                                        -- If it is a primitive, it is the same as an explicit casting
                                                                        then do
                                                                            case $3 of
                                                                                -- An argument is needed
                                                                                [] -> pushError $2 "Missing expression"
                                                                                -- but only *one* argument
                                                                                (_:_:_) -> pushError $2 "Too many arguments"
                                                                                _ -> return ()
                                                                            
                                                                            -- Return a casting expression (if needed)
                                                                            return $ castExp (head $3) $1

                                                                        -- If it is not a primitive
                                                                        else return $ AST.Literal' (reverse $3) $1 }

-- TODO: chequear que todos los elementos de la ListExp sean del mismo tipo.
LiteralMelody           :: { AST.Expression }
LiteralMelody           : '[' ListExp CloseSquare               {%do
                                                                    -- Error recovery
                                                                    pushIfError $3 $1 $ matchingError "square bracket"

                                                                    case $2 of
                                                                        [] -> return $ AST.LiteralMelody [] (AST.Simple "empty_list")
                                                                        (e:es) -> do
                                                                            let expType = AST.exp_type e
                                                                            if all (equalType expType) $ map AST.exp_type $2
                                                                                then                                                                           
                                                                                    return ()
                                                                                else pushError $1 "Not homogeneous melodies are not allowed:" 

                                                                            return $ AST.LiteralMelody $2 (AST.Compound "Melody" expType)}

Expression              :: { AST.Expression }
Expression              : LValue %prec LVALUE                   { $1 }
                        -- Boolean
                        | not Expression                        {%do
                                                                    let expected = AST.Simple "whole"
                                                                    checkExpType $2 [expected] $1
                                                                    return $ AST.NotExp $2 expected }
                        | Expression and Expression             {%do
                                                                    let expected = AST.Simple "whole"
                                                                    checkExpType $1 [expected] $2
                                                                    checkExpType $3 [expected] $2
                                                                    return $ AST.AndExp $1 $3 expected } 
                        | Expression or Expression              {%do
                                                                    let expected = AST.Simple "whole"
                                                                    checkExpType $1 [expected] $2
                                                                    checkExpType $3 [expected] $2
                                                                    return $ AST.OrExp $1 $3 expected } 

                        -- Aritmetivos
                        | '-' Expression %prec NEG              {%do
                                                                    checkExpType $2 AST.numberTypes $1
                                                                    return $ AST.NegativeExp $2 (AST.exp_type $2) }

                        | Expression '-' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.SubstractionExp exp1 exp2 finalType }
                        | Expression mod Expression             {%do
                                                                    checkExpType $1 [AST.Simple "quarter", AST.Simple "eighth"] $2
                                                                    checkExpType $3 [AST.Simple "quarter", AST.Simple "eighth"] $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.ModExp exp1 exp2 finalType }
                        | Expression '/' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.DivExp exp1 exp2 finalType }
                        | Expression '*' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.MultExp exp1 exp2 finalType }
                        | Expression '^' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 [AST.Simple "quarter", AST.Simple "eighth"] $2
                                                                    
                                                                    return $ AST.PowExp $1 $3 (AST.exp_type $1) }             
                        | Expression '+' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.AdditionExp exp1 exp2 finalType }

                        -- Relacionales
                        | Expression '=' Expression             {%do
                                                                    let expected = [AST.exp_type $1]
                                                                    checkEquality $1 $3 $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.EqualExp exp1 exp2 (AST.Simple "whole") }

                        | Expression '/=' Expression            {%do
                                                                    let expected = [AST.exp_type $1]
                                                                    checkEquality $1 $3 $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.NotEqualExp exp1 exp2 (AST.Simple "whole") }
                        | Expression '<' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.LessExp exp1 exp2 (AST.Simple "whole") }
                        | Expression '>' Expression             {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.GreaterExp exp1 exp2 (AST.Simple "whole") }
                        | Expression '<=' Expression            {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2

                                                                    let finalType = max (AST.exp_type $1) (AST.exp_type $3)
                                                                        exp1 = castExp $1 finalType
                                                                        exp2 = castExp $3 finalType

                                                                    return $ AST.LessEqualExp exp1 exp2 (AST.Simple "whole") }
                        | Expression '>=' Expression            {%do
                                                                    checkExpType $1 AST.numberTypes $2
                                                                    checkExpType $3 AST.numberTypes $2
                                                                    return $ AST.GreaterEqualExp $1 $3 (AST.Simple "whole") }

                        -- Micelaneos
                        | Literal                               { $1 }
                        | '(' Expression ClosePar               {% do
                                                                    pushIfError $3 $1 $ matchingError "parentheses"
                                                                    return $ $2 }

                        | new Literal                           { AST.NewExp (Just $2) (AST.Compound "Sample" (AST.exp_type $2)) }
                        | new IdType                            { AST.NewExp Nothing (AST.Compound "Sample" $2) }

                        | CallFuncion                           { $1 }

IdType                  :: { AST.Type }
IdType                  : id_type                               {%do
                                                                    entryMaybe <- PMonad.lookup (token $1)
                                                                    case entryMaybe of
                                                                        Nothing -> pushError $1 "Type not declared in scope:"
                                                                        Just _ -> return ()
                                                                    return $ AST.Simple (token $1) }

NewType                 :: { String }
NewType                 : chord IdType                         { AST.type_str $2 }
                        | legato IdType                        { AST.type_str $2 }

ChordLegato             :: { () }
ChordLegato             : NewType PushScope ChordLegatoFields PopScope  {% PMonad.updateEntry (addFields $3) $1 }

ChordLegatoFields       : '{' ListaField CloseBracket                    {% do
                                                                            pushIfError $3 $1 $ matchingError "bracket"
                                                                            return $ reverse $2 }

ListaField              :: { [AST.VarDeclaration] }
ListaField              : FieldDeclaration                      { [$1] }
                        | ListaField ',' FieldDeclaration       { $3 : $1 }

FieldDeclaration        :: { AST.VarDeclaration }
FieldDeclaration        : Id ':' Type                           {% do
                                                                    createFieldEntry (AST.id_token $1) $3
                                                                    return $ AST.VarDec $1 $3 }

PushScope               :: { () }
PushScope               : {- empty -}                           {% PMonad.pushScope }

PopScope                :: { () }
PopScope                : {- empty -}                           {% PMonad.popScope }

{

voidType = AST.Simple "void"
-----------------------------------------------------------------------------------------------
-- Exceptions
-----------------------------------------------------------------------------------------------

-- | Throws a syntatic error
parseError :: [Token] -> PMonad.ParserMonad a
parseError [] = error $ "Source file is not syntatically written well."
parseError (tk:_) = do
    srcFile <- RWS.ask
    throwCompilerError srcFile [Error (line tk) (col tk) "Parse error:"]

-----------------------------------------------------------------------------------------------
-- Entry Creation
-----------------------------------------------------------------------------------------------

createFunctionEntry :: Token -> Maybe AST.Type -> AST.Id -> [AST.VarDeclaration] -> Maybe AST.Block -> ParserMonad ()
createFunctionEntry tk semType funcId params block = do
    a <- verifyFunctionEntry (token tk) 
    if a then do 
        let funcat = SemData.Function { SemData.function_block = block, SemData.function_params = params }
        (PMonad.ParserState (SemData.Scopes _ (_:prev:_)) _ _ _ _) <- RWS.get
        
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = funcat,
            SemData.entry_scope = prev,
            SemData.entry_type = semType,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        pushError tk "Function already declared:"

createVarEntry :: Token -> Maybe AST.Type -> ParserMonad ()
createVarEntry tk semType = do
    curr <- PMonad.currScope
    result <- verifyVarEntry (token tk) curr
    if result then do 
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = SemData.Var,
            SemData.entry_scope = curr,
            SemData.entry_type = semType,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        pushError tk "Variable already declared in the same scope:"

createConstEntry :: Token -> Maybe AST.Type -> ParserMonad ()
createConstEntry tk semType = do 
    curr <- PMonad.currScope
    result <- verifyVarEntry (token tk) curr
    let typeconst = fromMaybe (AST.Simple "quarter") semType  
    if result then do 
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = SemData.Const,
            SemData.entry_scope = curr,
            SemData.entry_type = Just typeconst,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        pushError tk "Const already declared in the same scope:"

createTypeEntry :: Token -> String -> ParserMonad ()
createTypeEntry tk typeStr = do
    result <- verifyTypeEntry typeStr
    if result then do
        st@(PMonad.ParserState _ _ lvl _ _) <- RWS.get
        curr <- PMonad.currScope

        let entry = SemData.Entry {
            SemData.entry_name = typeStr,
            SemData.entry_category = (SemData.Type Nothing (Just $ getAdt tk)),
            SemData.entry_scope = curr,
            SemData.entry_type = Nothing,
            SemData.entry_level = Just (lvl+1)
        }
        PMonad.insertEntry entry
    else
        pushError tk "Type already declared in same scope:"
    where
        getAdt tk 
            | token tk == "chord"= SemData.Chord
            | token tk == "legato"= SemData.Legato

createFieldEntry :: Token -> AST.Type -> ParserMonad ()
createFieldEntry tk semType = do
    curr <- PMonad.currScope
    result <- verifyFieldEntry (token tk) curr
    entryMaybe <- PMonad.lookup (AST.type_str semType)
    if result then do
        case entryMaybe of
            Nothing -> pushError tk $ "Type " ++ show semType ++ " not declared in scope:"
            Just _ -> do
                let entry = SemData.Entry {
                    SemData.entry_name = token tk,
                    SemData.entry_category = SemData.Field,
                    SemData.entry_scope = curr,
                    SemData.entry_type = Just semType,
                    SemData.entry_level = Nothing
                }
                PMonad.insertEntry entry
    else
        pushError tk "Field already declared in same scope:"

createParamEntry :: Token -> Maybe AST.Type -> Bool -> ParserMonad ()
createParamEntry tk semType ref = do
    curr <- PMonad.currScope
    result <- verifyParamsEntry (token tk) curr
    if result then do
        let entry = SemData.Entry {
            SemData.entry_name = token tk,
            SemData.entry_category = SemData.Param ref,
            SemData.entry_scope = curr,
            SemData.entry_type = semType,
            SemData.entry_level = Nothing
        }
        PMonad.insertEntry entry
    else
        pushError tk "Parameter already defined in same track:"
    


        
-----------------------------------------------------------------------------------------------
-- Verification redeclarations
-----------------------------------------------------------------------------------------------
verifyFunctionEntry :: String -> ParserMonad (Bool)
verifyFunctionEntry name = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (SemData.Entry _ (SemData.Function _ _) _ _ _) -> return False
        Just _ -> return True

verifyVarEntry :: String -> Int -> ParserMonad (Bool)
verifyVarEntry name current_scope = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (SemData.Entry _ SemData.Var scope _ _) -> return $ scope /= current_scope
        Just (SemData.Entry _ SemData.Const scope _ _) -> return $ scope /= current_scope
        Just _ -> return True

verifyTypeEntry :: String -> ParserMonad (Bool)
verifyTypeEntry name = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (SemData.Entry _ (SemData.Type _ _) _ _ _) -> return False
        Just _ -> return True

verifyFieldEntry :: String -> Int -> ParserMonad (Bool)
verifyFieldEntry name current_scope = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (SemData.Entry _ SemData.Field scope _ _) -> return $ scope /= current_scope
        Just _ -> return True

verifyParamsEntry :: String -> Int -> ParserMonad (Bool)
verifyParamsEntry name current_scope = do
    entry <- PMonad.lookup name
    case entry of
        Nothing -> return True
        Just (SemData.Entry _ (SemData.Param _) scope _ _) -> return $ scope /= current_scope
        Just _ -> return True

addBlock :: AST.Block -> [SemData.Entry] -> Maybe [SemData.Entry]
addBlock block lst = Just $ map mapping lst
    where 
        condition e = 
            case SemData.entry_category e of
                SemData.Function Nothing _ -> True
                _ -> False
        mapping e = if condition e then e { SemData.entry_category = (SemData.entry_category e) { SemData.function_block = Just block } }
                else e

addFields :: [AST.VarDeclaration] -> [SemData.Entry] -> Maybe [SemData.Entry]
addFields listFields lst = Just $ map mapping lst
    where 
        condition entry = 
            case SemData.entry_category entry of
                SemData.Type Nothing _-> True
                _ -> False
        mapping entry = if condition entry then entry { SemData.entry_category = (SemData.entry_category entry) { SemData.type_fields = Just listFields } }
                else entry
--------------------------------------------
-----------Chequear tipos ------------------
--------------------------------------------

-- Chequea que ambos tipos sean compatibles
equalType :: AST.Type -> AST.Type -> Bool
equalType (AST.Compound ou inner) (AST.Compound ou' inner') =
    ou == ou' && equalType inner inner'
    
equalType (AST.Simple ou) (AST.Simple ou') =
    ou == ou'

equalType (AST.Compound "Melody" _) (AST.Simple "empty_list") = True
equalType (AST.Simple "empty_list") (AST.Compound "Melody" _) = True

equalType (AST.Compound "Sample" _) (AST.Simple "null") = True
equalType (AST.Simple "null") (AST.Compound "Sample" _) = True

equalType _ _ = False

-- | Chequea que el tipo de la expresión sea compatible con alguno de los tipos pasados como argumento.
checkExpType :: AST.Expression -> [AST.Type] -> Token -> ParserMonad AST.Type
checkExpType exp expected tk = do
    let expType = AST.exp_type exp

    if filter (equalType expType) expected == []
        then pushError tk $ "Expression isn't of type " ++ showExpected expected ++ ":"
        else return ()
    return expType

-- | Para imprimir bonito los tipos esperados
showExpected :: [AST.Type] -> String
showExpected [] = ""
showExpected [t] = show t
showExpected expected@(_:_:_) = let strs = map show expected in
    intercalate ", " (init strs) ++ " or " ++ (last strs)

-- | Chequea que la expresión derecha sea compatible para ser asignada con la expresión izquierda
checkAssignment :: AST.Expression -> AST.Expression -> Token -> ParserMonad ()
checkAssignment = checkAssignment' . AST.exp_type

-- | Chequea que el tipo de la expresión pueda ser asignado al tipo pasado por parámetro.
checkAssignment' :: AST.Type -> AST.Expression -> Token -> ParserMonad ()
checkAssignment' astType exp tk = do
    if astType `elem` AST.numberTypes
        then checkExpType exp (filter (<= astType) AST.numberTypes) tk
        else checkExpType exp [astType] tk
    return ()

-- | Chequea que los tipos sean comparables
checkEquality :: AST.Expression -> AST.Expression -> Token -> ParserMonad ()
checkEquality lexp rexp tk = do
    let astType = AST.exp_type lexp
    if astType `elem` AST.numberTypes
        then checkExpType rexp AST.numberTypes tk
        else checkExpType rexp [astType] tk
    return ()

-- | Retorna la expresión si es del tipo escogido, si no, retorna la expresión casteada.
castExp :: AST.Expression -> AST.Type -> AST.Expression
castExp exp finalType =
    if AST.exp_type exp == finalType 
        then exp
        else AST.CastExp exp (AST.exp_type exp) finalType 

-- | Chequear que los parametros tengan tipos compatibles con los argumentos y retorna una lista
-- de de los parametros casteados si no hubo un error.
checkParams :: AST.Id -> [AST.Expression] -> [AST.VarDeclaration] -> ParserMonad [AST.Expression]
checkParams id xs ys = checkParams' id xs ys []

checkParams' :: AST.Id -> [AST.Expression] -> [AST.VarDeclaration] -> [AST.Expression] -> ParserMonad [AST.Expression]
checkParams' id [] [] lst = return lst

checkParams' id xs [] _ = do
    pushError (AST.id_token id) "Too much arguments passed to the function"
    return [] 

checkParams' id [] ys lst = do
    pushError (AST.id_token id) "Too few arguments passed to the function"
    return []

checkParams' id (x:xs) (y:ys) lst = do
    let ytype = AST.var_type y
    checkAssignment' ytype x (AST.id_token id)
    checkParams' id xs ys ((castExp x ytype):lst)

notDeclaration = \inst -> case inst of 
    AST.VarDecInst _ -> False 
    _ -> True

checkFieldsChord :: Token -> [AST.Expression] -> Maybe [AST.VarDeclaration] -> ParserMonad ()
checkFieldsChord tk _ Nothing = pushError tk "No arguments have been passed to the constructor:"
checkFieldsChord tk [] (Just []) = return ()
checkFieldsChord tk [] (Just ys) = pushError tk "Too few arguments have been passed to the constructor:"
checkFieldsChord tk xs (Just []) = pushError tk "Too much arguments have been passed to the constructor:"
checkFieldsChord tk (x:xs) (Just (y:ys)) = do
    let ytype = AST.var_type y
    xtype <- checkExpType x [ytype] tk
    checkFieldsChord tk xs (Just ys)

checkFieldsLegato :: Token -> [AST.Expression] -> Maybe [AST.VarDeclaration] -> ParserMonad ()
checkFieldsLegato tk _ Nothing = pushError tk "No arguments have been passed to the constructor:"
checkFieldsLegato tk [] (Just []) = return ()
checkFieldsLegato tk [] (Just ys) = pushError tk "Too few arguments have been passed to the constructor:"
checkFieldsLegato tk [x] (Just ys) = do
    let ytypes = map AST.var_type ys
    xtype <- checkExpType x ytypes tk
    return ()
checkFieldsLegato tk (x:x':xs) _ = pushError tk "Too much arguments have been passed to the constructor:"

--------------------------------------------
------------Error Recovery------------------
--------------------------------------------

matchingError tkString = "Couldnt match closing " ++ tkString ++ ":"

pushIfError :: Bool -> Token -> String -> ParserMonad ()
pushIfError good tk msg =
    if (not good)
        then PMonad.pushError $ Error (line tk) (col tk) msg
        else return ()

pushError :: Token -> String -> ParserMonad ()
pushError = pushIfError False

--------------------------------------------
----------------- END ----------------------
--------------------------------------------
}