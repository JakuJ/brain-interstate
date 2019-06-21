module Main where

import Common (query)
import Interpret (runInterpreter, Tape, makeTape)

repl :: Tape -> IO ()
repl tape = do
    print tape
    line <- query "> "
    repl =<< runInterpreter line tape

main :: IO ()
main = do
    n <-read <$> query "Specify tape size: "
    repl $ makeTape n
