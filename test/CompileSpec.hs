module CompileSpec where

import System.IO (writeFile)
import System.Directory (removeFile)
import System.Process (callProcess, readProcess)
import System.Environment (withArgs)
    
import Test.Hspec
import Compile (runCompiler)
    
writeTmp :: String -> IO ()
writeTmp = writeFile "test-source.txt"

cleanup :: IO ()
cleanup = do
    removeFile "test-source.txt"
    removeFile "test.exe"

testCompile :: String -> IO ()
testCompile source = do
    writeTmp source
    withArgs ["-o", "test.exe", "test-source.txt"] runCompiler

testRun :: String -> IO String
testRun input = do
    callProcess "chmod" ["+x", "./test.exe"]
    readProcess "./test.exe" [] input

spec :: Spec
spec = after_ cleanup $ do
    describe "Compile.runCompiler" $ do
        context "when given good filepaths" $ do
            it "compiles and runs a hello world" $ do
                testCompile helloWorld
                testRun "" `shouldReturn` "Hello World!\n"
            it "compiles and runs a cat program" $ do
                testCompile cat
                let testString = "abcDEF123"
                testRun (testString) `shouldReturn` testString
            it "compiles and runs a digit factorial calculator" $ do
                testCompile factorial
                testRun "5" `shouldReturn` "x" -- 'x' is ASCII for 120 (5 factorial)
            it "compiles and tests a brainfuck interpreter" $ do
                testCompile interpreter
                let testString = factorial ++ "!" ++ "5"
                testRun testString `shouldReturn` "x"
            it "compiles and tests a brainfuck to C translator" $ do
                testCompile bfToCTransaltor
                hwCode <- testRun (helloWorld ++ "x")
                writeFile "test-source.c" hwCode
                callProcess "gcc" ["-w", "-o", "test.exe", "test-source.c"]
                removeFile "test-source.c"
                testRun "" `shouldReturn` "Hello World!\n"
            

-- Brainfuck programs
-- |A program that prints "Hello World!\n"
helloWorld :: String
helloWorld = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

-- |A cat program, repeats its input
cat :: String
cat = ",[.,]"

-- |Reads a digit X and prints a character with ASCII code of X
factorial :: String
factorial = "++++++>,<[->--------<]>[[>>+>+<<<-]>>>-]+<[>[<[<+<+>>-]<[>+<-]>>-]<<<<]>."

-- |Interprets BF code followed by '!' and its input [Source: http://www.hevanet.com/cristofd/brainfuck/]
interpreter :: String
interpreter = "\
\>>>+[[-]>>[-]++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[\
\->>]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<\
\]<]<[[<]>[[>]>>[>>]+[<<]<[<]<+>>-]>[>]+[->>]<<<<[[<<]<[<]+<<[+>+<<-[>-->+<<-[>\
\+<[>>+<<-]]]>[<+>-]<]++>>-->[>]>>[>>]]<<[>>+<[[<]<]>[[<<]<[<]+[-<+>>-[<<+>++>-\
\[<->[<<+>>-]]]<[>+<-]>]>[>]>]>[>>]>>]<<[>>+>>+>>]<<[->>>>>>>>]<<[>.>>>>>>>]<<[\
\>->>>>>]<<[>,>>>]<<[>+>]<<[+<<]<]"

-- |Translates input BF code to C code [Source: http://www.hevanet.com/cristofd/brainfuck/]
bfToCTransaltor :: String
bfToCTransaltor = "\
\>+++++[>+++++++<-]>.<<++[>+++++[>+++++++<-]<-]>>.+++++.<++[>-----<-]>-.<++\
\[>++++<-]>+.<++[>++++<-]>+.[>+>+>+<<<-]>>>[<<<+>>>-]<<<<<++[>+++[>---<-]<-\
\]>>+.+.<+++++++[>----------<-]>+.<++++[>+++++++<-]>.>.-------.-----.<<++[>\
\>+++++<<-]>>.+.----------------.<<++[>-------<-]>.>++++.<<++[>++++++++<-]>\
\.<++++++++++[>>>-----------<<<-]>>>+++.<-----.+++++.-------.<<++[>>+++++++\
\+<<-]>>+.<<+++[>----------<-]>.<++[>>--------<<-]>>-.------.<<++[>++++++++\
\<-]>+++.---....>++.<----.--.<++[>>+++++++++<<-]>>+.<<++[>+++++++++<-]>+.<+\
\+[>>-------<<-]>>-.<--.>>.<<<+++[>>++++<<-]>>.<<+++[>>----<<-]>>.++++++++.\
\+++++.<<++[>---------<-]>-.+.>>.<<<++[>>+++++++<<-]>>-.>.>>>[-]>>[-]<+[<<[\
\-],[>>>>>>>>>>>>>+>+<<<<<<<<<<<<<<-]>>>>>>>>>>>>>>[<<<<<<<<<<<<<<+>>>>>>>>\
\>>>>>>-]<<+>[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-[-\
\[-[-[-[-[-[-[-[-[-[-[-[<->[-]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]<[\
\<<<<<<<<<<<<[-]>>>>>>>>>>>>[-]]<<<<<<<<<<<<[<+++++[>---------<-]>++[>]>>[>\
\+++++[>+++++++++<-]>--..-.<+++++++[>++++++++++<-]>.<+++++++++++[>-----<-]>\
\++.<<<<<<.>>>>>>[-]<]<<<[-[>]>>[>++++++[>+++[>++++++<-]<-]>>++++++.-------\
\------.----.+++.<++++++[>----------<-]>.++++++++.----.<++++[>+++++++++++++\
\++++<-]>.<++++[>-----------------<-]>.+++++.--------.<++[>+++++++++<-]>.[-\
\]<<<<<<<.>>>>>]<<<[-[>]>>[>+++++[>+++++++++<-]>..---.<+++++++[>++++++++++<\
\-]>.<+++++++++++[>-----<-]>++.<<<<<<.>>>>>>[-]<]<<<[-[>]>>[>+++[>++++[>+++\
\+++++++<-]<-]>>-.-----.---------.<++[>++++++<-]>-.<+++[>-----<-]>.<++++++[\
\>----------<-]>-.<+++[>+++<-]>.-----.<++++[>+++++++++++++++++<-]>.<++++[>-\
\----------------<-]>.+++++.--------.<++[>+++++++++<-]>.[-]<<<<<<<.>>>>>]<<\
\<[<+++[>-----<-]>+[>]>>[>+++++[>+++++++++<-]>..<+++++++[>++++++++++<-]>---\
\.<+++++[>----------<-]>---.<<<<<<.>>>>>>[-]<]<<<[--[>]>>[>+++++[>+++++++++\
\<-]>--..<+++++++[>++++++++++<-]>-.<+++++[>----------<-]>---.[-]<<<<<<.>>>>\
\>]<<<[<+++[>----------<-]>+[>]>>[>+++[>++++[>++++++++++<-]<-]>>-.<+++[>---\
\--<-]>.+.+++.-------.<++++++[>----------<-]>-.++.<+++++++[>++++++++++<-]>.\
\<+++++++[>----------<-]>-.<++++++++[>++++++++++<-]>++.[-]<<<<<<<.>>>>>]<<<\
\[--[>]>>[>+++++[>+++++[>+++++<-]<-]>>.[-]<<<<<<<.>>>>>]<<<[<++++++++++[>--\
\--------------<-]>--[>]>>[<<<<[-]]]]]]]]]]]>>]<++[>+++++[>++++++++++<-]<-]\
\>>+.<+++[>++++++<-]>+.<+++[>-----<-]>.+++++++++++.<+++++++[>----------<-]>\
\------.++++++++.-------.<+++[>++++++<-]>.<++++++[>+++++++++++<-]>.<+++++++\
\+++."