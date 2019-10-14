module FunctionSpec where

    import Test.Hspec
    import Lexer
    import Utils
    import Tokens
    
    spec :: Spec
    spec = describe "Lexer" $ do
    
        it "accepts `0` as a valid int literal" $ do
            let x = "track f(n: quarter): half"
            let atok = getTok $ runAlexScan x    
            case atok of
                [_, IdToken idf _ _, _, IdToken idn _ _, _, QuarterToken tkq _ _, _, _, HalfToken tkh _ _ ] -> do
                    idf `shouldBe` "f"
                    idn `shouldBe` "n"
                    tkq `shouldBe` "quarter"
                    tkh `shouldBe` "half"
                _ -> error "Pattern failed"
    
        -- it "accepts `1` as a valid int literal" $ do
        --     let x = "1"
        --     s <- runAlexScan x
        --     case s of
        --         Just toks -> do
        --             let atok = getToken $ head toks
        --             atok `shouldBe` IntToken
        --         Nothing ->
        --             error "rejected as an invalid token"
    
        -- it "accepts `100` as a valid int literal" $ do
        --     let x = "100"
        --     s <- runAlexScan x
        --     case s of
        --         Just toks -> do
        --             let atok = getToken $ head toks
        --             atok `shouldBe` IntToken
        --         Nothing ->
        --             error "rejected as an invalid token"
    