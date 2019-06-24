module Translate (
    translateProgram
) where

import Data.List (intercalate, dropWhile)
import Parse (Node (..), parseProgram)

runtimeStart :: String
runtimeStart = "#include <stdlib.h>\n\
\#include <stdio.h>\n\
\int main(int argc, char* argv[])\n\
\{\n\
\int *tape = calloc(20000, sizeof(int));\n\
\int *p = tape;\n"

runtimeEnd :: String
runtimeEnd = "free(tape);\nreturn 0;\n}\n"

translate :: Node -> String
translate (Plus n) = "(*p) += " ++ show n ++ ";\n"
translate (Minus n) = "(*p) -= " ++ show n ++ ";\n"
translate (MoveRight n) = "p += " ++ show n ++ ";\n"
translate (MoveLeft n) = "p -= " ++ show n ++ ";\n"
translate Input = "if((*p = getchar()) == EOF) *p = 0;\n"
translate Output = "putchar(*p);\n"
translate (Loop nodes) = "while(*p)\n{\n" ++ (unlines . map ('\t' :). lines . concatMap translate) nodes ++ "}\n"

translateProgram :: String -> String
translateProgram code = runtimeStart ++ concatMap translate nodes ++ runtimeEnd
    where 
        nodes = case parseProgram code of
            Right ns -> ns
            Left err -> error $ "Couldn't parse code: " ++ err