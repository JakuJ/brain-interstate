{-# LANGUAGE LambdaCase #-}

module Interpret
( runInterpreter, Tape, makeTape )
where

import Data.Char (ord, chr)
import Data.Array (Array, listArray, elems, (!), (//))
import Control.Monad.State

import Common (query)
import Parse (Node (..), parseProgram)

data Tape = Tape {index :: Int, tape :: Array Int Int}

instance Show Tape where
    show (Tape ix arr) = show ix ++ " @ " ++ (show . elems) arr

makeTape :: Int -> Tape
makeTape n = Tape 1 $ listArray (1, n) $ repeat 0

-- Tape manipulation

replace :: Int -> Tape -> Tape
replace x t@(Tape ix arr) = Tape ix $ arr // [(ix, x)]

current :: Tape -> Int
current (Tape ix arr) = arr ! ix

changeBy :: Int -> State Tape ()
changeBy n = do
    cur <- gets current
    modify $ replace $ cur + n

jump :: Int -> State Tape ()
jump n  = do
    (Tape ix arr) <- get
    put $ Tape (ix + n) arr

-- Basic State manipulation

eval :: Node -> State Tape ()
eval = \case
    Plus n -> changeBy n
    Minus n -> changeBy (-n)
    MoveRight n -> jump n
    MoveLeft n -> jump (-n)
    _ -> fail "Node type not supported"

-- IO State manipulation

printOut :: StateT Tape IO ()
printOut = do
    x <- gets current
    liftIO $ print $ chr x

getInput :: StateT Tape IO ()
getInput = do
    x <- liftIO $ query "Enter a character: "
    if not $ null x
        then
            modify $ replace $ (ord . head) x
        else
            liftIO $ putStrLn "ERROR: No character provided"

evalIO :: Node -> StateT Tape IO ()
evalIO n = case n of
    Input -> getInput
    Output -> printOut
    Loop nodes -> do
        cur <- gets current
        unless (cur == 0) $ do
            interpret nodes
            evalIO n
    _ -> modify $ execState $ eval n

-- Actual interpreting

interpret :: [Node] -> StateT Tape IO ()
interpret = foldl (\prev next -> prev >> evalIO next) $ return ()

runInterpreter :: String -> Tape -> IO Tape
runInterpreter str tape = case parseProgram str of
    Right nodes -> execStateT (interpret nodes) tape
    Left error -> putStrLn ("ERROR: " ++ error) >> return tape