module FunctionSpec where

import Test.Hspec
import Frontend.Lexer
import Utils
import Frontend.Tokens

spec :: Spec
spec = describe "Lexer.Functions" $ do

    it "accepts `track f(n: quarter): half` as a valid Function Token List" $ do
        let x = "track f(n: quarter): half"
        let atok = getTok $ runAlexScan x    
        case atok of
            [_, IdToken idf _ _, _, IdToken idn _ _, _, QuarterToken tkq _ _, _, _, HalfToken tkh _ _ ] -> do
                idf `shouldBe` "f"
                idn `shouldBe` "n"
                tkq `shouldBe` "quarter"
                tkh `shouldBe` "half"
            _ -> error "Pattern failed"
    
    it "Accepts `track f():whole` as a valid Function Token List" $ do
        let x = "track f():whole"
        let atok = getTok $ runAlexScan x
        case atok of
            [_, IdToken idf _ _, _, _, _, WholeToken tkh _ _ ] -> do
                idf `shouldBe` "f"
                tkh `shouldBe` "whole"
            _ -> error "Pattern failed"

    it "Accepts `track f()` as a valid Function Token List" $ do
        let x = "track f()"
        let atok = getTok $ runAlexScan x
        case atok of
            [_, IdToken idf _ _, _, _] -> do
                idf `shouldBe` "f"
            _ -> error "Pattern failed"