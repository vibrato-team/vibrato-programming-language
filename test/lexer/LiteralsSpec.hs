module LiteralsSpec where

import Test.Hspec
import Lexer
import Utils
import Tokens

spec :: Spec
spec = describe "Lexer.literals" $ do

    describe "Numbers" $ do
        it "accepts `0` as a valid int literal" $ do
            let x = "0"
            let atok = getTok $ runAlexScan x    
            case atok of
                [IntToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"
        
        it "accepts `99999999` as a valid int literal" $ do
            let x = "99999999"
            let atok = getTok $ runAlexScan x    
            case atok of
                [IntToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `010` as a valid int literal" $ do
            let x = "010"
            let atok = getTok $ runAlexScan x    
            case atok of
                [IntToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `10.0` as a valid int literal" $ do
            let x = "10.0"
            let atok = getTok $ runAlexScan x    
            case atok of
                [FloatToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `0.1` as a valid int literal" $ do
            let x = "0.1"
            let atok = getTok $ runAlexScan x    
            case atok of
                [FloatToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"

    describe "Char and String" $ do
        it "accepts `a` as a valid char" $ do
            let x = "'a'"
            let atok = getTok $ runAlexScan x    
            case atok of
                [CharToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `\\n` as a valid char" $ do
            let x = "'\\n'"
            let atok = getTok $ runAlexScan x    
            case atok of
                [CharToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `\\0` as a valid char" $ do
            let x = "'\\0'"
            let atok = getTok $ runAlexScan x    
            case atok of
                [CharToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `\\\\` as a valid char" $ do
            let x = "'\\'"
            let atok = getTok $ runAlexScan x    
            case atok of
                [CharToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `aa` as a valid String" $ do
            let x = "\"aa\""
            let atok = getTok $ runAlexScan x    
            case atok of
                [StringToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `esto Es un bUEn string 123$%#$%#$@%&*()` as a valid String" $ do
            let x = "\"esto Es un bUEn string 123$%#$%#$@%&*()\""
            let atok = getTok $ runAlexScan x    
            case atok of
                [StringToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"

    describe "Booleans" $ do
        it "accepts `maj` as a valid Bool" $ do
            let x = "maj"
            let atok = getTok $ runAlexScan x    
            case atok of
                [MajToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"

        it "accepts `min` as a valid Bool" $ do
            let x = "min"
            let atok = getTok $ runAlexScan x    
            case atok of
                [MinToken tk _ _ ]-> tk `shouldBe` x
                _ -> error "Pattern failed"