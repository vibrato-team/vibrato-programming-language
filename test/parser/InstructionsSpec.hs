module InstructionsSpec where

import Test.Hspec
import Tokens
import Utils
import AST
import Data.Maybe


buildProgramWithLiteral :: String -> String
buildProgramWithLiteral l = "\
\ moderato(){ \
\ " ++ l ++ " \
\ }"

buildProgramWithLiteralAndFunction :: String -> String-> String
buildProgramWithLiteralAndFunction f l = "\
\ track f(){\
\ " ++ f ++ " \
\}\
\ moderato(){ \
\ " ++ l ++ " \
\ }"


spec :: Spec
spec = describe "Parser.Instructions" $ do
    it "accepts `moderato(){ || }` as a block without statements" $ do
        program <- testValidProgram (buildProgramWithLiteral "a: quarter |")
        program `shouldBe` Block {statements = []}

    it "accepts `moderato(){ a:quarter }` as a block without statements" $ do
        program <- testValidProgram (buildProgramWithLiteral "a: quarter |")
        program `shouldBe` Block {statements = []}

    it "accepts `moderato(){ a:quarter <-> 10}` as a block with return" $ do
        program <- testValidProgram (buildProgramWithLiteral "a: quarter <-> 10 |")
        
        program `shouldBe` Block [AssignInst (IdExp (Id (IdToken "a" 1 15)) (Simple "quarter")) (Literal (IntToken "10" 1 30) (Simple "quarter"))]

    it "accepts `moderato(){ a:quarter | a <-> 10 |}` as a block with Assign Instruction" $ do
        program <- testValidProgram (buildProgramWithLiteral "a: quarter | a <-> 10 |")
        program `shouldBe` Block [AssignInst (IdExp (Id (IdToken "a" 1 28)) (Simple "quarter")) (Literal (IntToken "10" 1 34) (Simple "quarter"))]

    it "accepts `moderato(){ play f }` as a block with Call Function Instruction" $ do
        program <- testValidProgram (buildProgramWithLiteralAndFunction "|>(10) |" "play f |")
        head (statements program) `shouldBe` CallFuncInst (CallExp (Id (IdToken "f" 1 42)) [] (Simple "void"))

    it "accepts `moderato(){ |>(10) }` as a block with Play Instruction" $ do
        program <- testValidProgram (buildProgramWithLiteral "|>(10) |")
        head (statements program) `shouldBe` PlayInst [Literal (IntToken "10" 1 18) (Simple "quarter")]

    it "accepts `moderato(){a:quarter | @(a) }` as a block with Record Instruction" $ do
        program <- testValidProgram (buildProgramWithLiteral "a:quarter | @(a) |")
        head (statements program) `shouldBe` RecordInst [IdExp (Id (IdToken "a" 1 29)) (Simple "quarter")]

    it "accepts `moderato(){ if(maj){a:quarter} }` as a block with If Instruction" $ do
        program <- testValidProgram (buildProgramWithLiteral "if(maj){a:quarter |}")
        head (statements program) `shouldBe` IfInst (IdExp (Id (MajToken "maj" 1 18)) (Simple "whole")) (BlockInst (Block [])) Nothing

    it "accepts `moderato(){ loop(maj){a:quarter |} }` as a block with While Instruction" $ do
        program <- testValidProgram (buildProgramWithLiteral "loop(maj){a:quarter |}")
        head (statements program) `shouldBe` WhileInst (IdExp (Id (MajToken "maj" 1 20)) (Simple "whole")) (Block [])

    -- it "accepts `moderato(){ || }` as a block with" $ do
    --     program <- testValidProgram (buildProgramWithLiteral "a: quarter |")
    --     program `shouldBe` Just (Block {statements = []})

    -- it "accepts `moderato(){ || }` as a block with" $ do
    --     program <- testValidProgram (buildProgramWithLiteral "a: quarter |")
    --     program `shouldBe` Just (Block {statements = []})

    -- it "accepts `moderato(){ || }` as a block with" $ do
    --     program <- testValidProgram (buildProgramWithLiteral "a: quarter |")
    --     program `shouldBe` Just (Block {statements = []})

    -- it "accepts `moderato(){ || }` as a block with" $ do
    --     program <- testValidProgram (buildProgramWithLiteral "a: quarter |")
    --     program `shouldBe` Just (Block {statements = []})