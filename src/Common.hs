module Common (query) where

import System.IO (hFlush, stdout)

query :: String -> IO String
query str = putStr str >> hFlush stdout >> getLine
