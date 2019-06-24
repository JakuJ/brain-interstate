module Compile (
    runCompiler
) where

import Data.List (intercalate)
import Control.Monad (forM_, unless)
import System.Environment (getArgs)
import System.Process (callProcess)
import System.FilePath.Posix (takeFileName, takeBaseName)

import Argparse (Argument (..), argparse, path, isSingle, fromOption)
import Translate (translateProgram)

compileFromAs :: FilePath -> [Argument] -> IO ()
compileFromAs path args = do
    compiledCode <- translateProgram <$> (readFile path)
    writeFile sourceFile compiledCode
    callProcess "gcc" ["-O3", "-o", execName, sourceFile]
    unless (Flag "S" `elem` args) $ callProcess "rm" [sourceFile]
        where
            programName, execName, sourceFile :: String
            programName = takeFileName path
            execName = fromOption "o" args $ programName ++ ".out"
            sourceFile = takeBaseName execName ++ ".c"

runCompiler :: IO ()
runCompiler = do
    parsedArgs <- (argparse . intercalate " ") <$> getArgs
    case parsedArgs of
        Left err -> fail err
        Right args -> do
            let sources = map path . filter isSingle $ args
            forM_ sources $ \filepath -> compileFromAs filepath args