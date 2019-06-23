module CodeParseSpec where

import Test.Hspec
import Data.Either (isRight)
import Parse (Node, parseProgram)

goodSyntaxTestCases:: [String]
goodSyntaxTestCases = ["", "+-.,<>", "+++[+]---", "+[>><++-,][+][-]", "+++ [.] -., comment"]

spec :: Spec
spec = do
    describe "Parse.parseProgram" $ do
        it "Properly parses good grammar" $ do
            mapM_ (\t -> parseProgram t `shouldSatisfy` isRight) goodSyntaxTestCases
        it "Catches invalid syntax errors" $ do
            parseProgram "[" `shouldBe` Left "Missing right bracket"
            parseProgram "]" `shouldBe` Left "Missing left bracket"
            parseProgram "+++[--[.." `shouldBe` Left "Missing right bracket"
            parseProgram "+[+[]-]-[" `shouldBe` Left "Infinite loop detected"
            parseProgram "[]" `shouldBe` Left "Infinite loop detected"
            
