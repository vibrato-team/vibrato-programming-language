module OperatorsSpec where

import Test.Hspec
import Lexer
import Utils
import Tokens

spec :: Spec
spec = describe "Lexer.operators" $ do

    describe "Dereference" $ do

        it "accepts `!` as a valid Dereference" $ do
            let x = "!"
            let atok = getTok $ runAlexScan x    
            case atok of
                [DereferenceToken tk _ _] -> do
                    tk `shouldBe` x
                _ -> error "Pattern failed"

    describe "Logical" $ do
            
        it "accepts `not` as a valid Not Logical" $ do
            let x = "not"
            let atok = getTok $ runAlexScan x    
            case atok of
                [NotToken tk _ _] -> do
                    tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `and` as a valid And Logical" $ do
            let x = "and"
            let atok = getTok $ runAlexScan x    
            case atok of
                [AndToken tk _ _] -> do
                    tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `or` as a valid Or Logical" $ do
            let x = "or"
            let atok = getTok $ runAlexScan x    
            case atok of
                [OrToken tk _ _] -> do
                    tk `shouldBe` x
                _ -> error "Pattern failed"

    describe "Arithmetics" $ do

        it "accepts `-1` as a valid Minus and Int Literal" $ do
            let x = "-1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [MinusToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"
    
        it "accepts `+1` as a valid Plus and Int Literal" $ do
            let x = "+1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [PlusToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"

        it "accepts `mod 1` as a valid Mod and Int Literal" $ do
            let x = "mod 1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [ModToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"
    
        it "accepts `*1` as a valid Mult and Int Literal" $ do
            let x = "*1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [MultToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"

        it "accepts `/1` as a valid Div and Int Literal" $ do
            let x = "/1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [DivToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"
    
        it "accepts `^1` as a valid Pow and Int Literal" $ do
            let x = "^1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [PowToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"
            
    describe "Comparison" $ do

        it "accepts `=1` as a valid Equal and Int Literal" $ do
            let x = "=1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [EqualToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"
    
        it "accepts `/=1` as a valid NotEqual and Int Literal" $ do
            let x = "/=1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [NotEqualToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"

        it "accepts `<1` as a valid Less and Int Literal" $ do
            let x = "<1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [LessToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"
    
        it "accepts `>1` as a valid Greater and Int Literal" $ do
            let x = ">1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [GreaterToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"

        it "accepts `<=1` as a valid LessEqual and Int Literal" $ do
            let x = "<=1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [LessEqualToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"
    
        it "accepts `>=1` as a valid GreaterEqual and Int Literal" $ do
            let x = ">=1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [GreaterEqualToken _ _ _, IntToken tk _ _] -> do
                    tk `shouldBe` "1"
                _ -> error "Pattern failed"

    describe "Index" $ do

        it "accepts `[` as a valid BracketOpen and Int Literal" $ do
            let x = "["
            let atok = getTok $ runAlexScan x    
            case atok of
                [BracketOpenToken tk _ _] -> do
                    tk `shouldBe` x
                _ -> error "Pattern failed"
    
        it "accepts `]` as a valid BracketClose and Int Literal" $ do
            let x = "]"
            let atok = getTok $ runAlexScan x    
            case atok of
                [BracketCloseToken tk _ _] -> do
                    tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `.` as a valid Dot and Int Literal" $ do
            let x = "."
            let atok = getTok $ runAlexScan x    
            case atok of
                [DotToken tk _ _] -> do
                    tk `shouldBe` x
                _ -> error "Pattern failed"

        