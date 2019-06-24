module ArgparseSpec where

import Test.Hspec
import Data.Either (isLeft)
import Argparse (argparse, Argument (..))

spec :: Spec
spec = parallel $ do
    describe "Argparse.argparse" $ do
        context "when given good input" $ do
            it "parses flags" $ do
                argparse "-a" `shouldBe` Right [Flag "a"]
                argparse "-a -b -c" `shouldBe` Right [Flag "a", Flag "b", Flag "c"]
                argparse "--abc" `shouldBe` Right [Flag "abc"]
                argparse "--abc-def" `shouldBe` Right [Flag "abc-def"]
                argparse "--abc --def" `shouldBe` Right [Flag "abc", Flag "def"]
            it "parses options with bare arguments" $ do
                argparse "--path /root/a/b/c.txt" `shouldBe` Right [Option "path" "/root/a/b/c.txt"]
                argparse "--local-path ./testfile" `shouldBe` Right [Option "local-path" "./testfile"]
                argparse "-p ../upper.exe" `shouldBe` Right [Option "p" "../upper.exe"]
                argparse "-a 123" `shouldBe` Right [Option "a" "123"]
            it "parses options with quoted arguments" $ do
                argparse "--path '/root/test folder/file.xdd'" `shouldBe` Right [Option "path" "/root/test folder/file.xdd"]
                argparse "--local-path './testfile'" `shouldBe` Right [Option "local-path" "./testfile"]
                argparse "-p \"../upper.exe\"" `shouldBe` Right [Option "p" "../upper.exe"]
                argparse "-p \"../upper space.exe\"" `shouldBe` Right [Option "p" "../upper space.exe"]
            it "allows for mixed quotes" $ do
                argparse "--option \"abc'def\"" `shouldBe` Right [Option "option" "abc'def"]
                argparse "--option 'abc\"def'" `shouldBe` Right [Option "option" "abc\"def"]
            it "parses single values" $ do
                argparse "abc" `shouldBe` Right [Single "abc"]
                argparse "abc def ghi" `shouldBe` Right [Single "abc", Single "def", Single "ghi"]
            it "parses multiple types of arguments" $ do
                argparse "-a --bc-de12" `shouldBe` Right [Flag "a", Flag "bc-de12"]
                argparse "-a --bc test" `shouldBe` Right [Flag "a", Option "bc" "test"]
                argparse "-a test1.txt --bc test -v -e" `shouldBe` Right [Option "a" "test1.txt", Option "bc" "test", Flag "v", Flag "e"]
                argparse "a -a --a a a a -a a" `shouldBe` Right [Single "a", Flag "a", Option "a" "a", Single "a", Single "a", Option "a" "a"]
                argparse "./hello-world.bf -o output" `shouldBe` Right [Single "./hello-world.bf", Option "o" "output"]

        context "when given bad input" $ do
            it "detects empty input" $ do
                argparse "" `shouldSatisfy` isLeft
            it "detects invalid flags" $ do
                argparse "---a" `shouldSatisfy` isLeft
                argparse "-abc" `shouldSatisfy` isLeft
            it "fails with flags with invalid number placement" $ do
                argparse "-a1" `shouldSatisfy` isLeft
                argparse "-1" `shouldSatisfy` isLeft
                argparse "--abc12def" `shouldSatisfy` isLeft
                argparse "--1" `shouldSatisfy` isLeft
            it "fails with quote mismatches" $ do
                argparse "-v 'abc'def" `shouldSatisfy` isLeft
                argparse "-b 'abc def'abc'" `shouldSatisfy` isLeft
