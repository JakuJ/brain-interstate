{-# LANGUAGE ScopedTypeVariables #-}

module Compiler (
    main
) where

import System.Environment (getProgName)
import Control.Exception (SomeException, catch)

import Compile (runCompiler)

usage :: IO ()
usage = do
    name <- getProgName
    putStrLn $ "Usage:\t" ++ name ++ " [-S] [-o <path>] <source file path>"
    putStrLn "Options:"
    mapM_ (putStrLn . ('\t':)) ["-S - don't remove the generated .c file",
                                "-o <path> - redirect output to file"]

main :: IO ()
main = catch runCompiler $ \(err :: SomeException) -> print err >> usage