module InstructionsSpec where

import Test.Hspec
import Lexer
import Utils
import Tokens

spec :: Spec
spec = describe "Lexer.Instructions" $ do

    it "accepts `x <-> 1000` as a valid Assigment Token" $ do
        let x = "x <-> 1000"
        let atok = getTok $ runAlexScan x    
        case atok of
            [IdToken tk _ _, AssignToken _ _ _, IntToken tl _ _ ]->do 
                tk `shouldBe` "x"
                tl `shouldBe` "1000"
            _ -> error "Pattern failed"

    it "accepts `{ }` as a valid block" $ do
        let x = "{ }"
        let atok = getTok $ runAlexScan x    
        case atok of
            [OpenCurlyToken tk _ _, CloseCurlyToken tl _ _]->do 
                tk `shouldBe` "{"
                tl `shouldBe` "}"
            _ -> error "Pattern failed"

    it "accepts `@ x | |> 100` as a valid IO Secc" $ do
        let x = "@ x | |> 100"
        let atok = getTok $ runAlexScan x    
        case atok of
            [RecordToken _ _ _, IdToken tk _ _, BarToken _ _ _, PlaySymToken _ _ _, IntToken tl _ _] -> do 
                tk `shouldBe` "x"
                tl `shouldBe` "100"
            _ -> error "Pattern failed"

    it "accepts `if else` as a valid Conditionals" $ do
        let x = "if else"
        let atok = getTok $ runAlexScan x    
        case atok of
            [IfToken tk _ _, ElseToken tl _ _]->do 
                tk `shouldBe` "if"
                tl `shouldBe` "else"
            _ -> error "Pattern failed"

    it "accepts `loop in >> |]` as a valid Cicles" $ do
        let x = "loop in >> |]"
        let atok = getTok $ runAlexScan x    
        case atok of
            [LoopToken tk _ _, InToken _ _ _, NextToken _ _ _, StopToken _ _ _]->do 
                tk `shouldBe` "loop"
            _ -> error "Pattern failed"
    
    it "accepts `# &` as a valid Conditionals" $ do
        let x = "#&"
        let atok = getTok $ runAlexScan x    
        case atok of
            [SharpToken tk _ _, FlatToken tl _ _]->do 
                tk `shouldBe` "#"
                tl `shouldBe` "&"
            _ -> error "Pattern failed"

    it "accepts `new free` as a valid Cicles" $ do
        let x = "new free"
        let atok = getTok $ runAlexScan x    
        case atok of
            [NewToken tk _ _, FreeToken _ _ _]->do 
                tk `shouldBe` "new"
            _ -> error "Pattern failed"