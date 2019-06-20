module Interpret
( runInterpreter, Tape, makeTape )
where

import Data.Char (ord, chr)
import Data.Array (Array, listArray, elems, (!), (//))
import Control.Monad.State

import Common (query)

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

jump :: Int -> Tape -> Tape
jump n (Tape ix arr) = Tape (ix + n) arr

-- Basic State manipulation

increment :: State Tape ()
increment = do
    cur <- gets current
    modify $ replace $ cur + 1

decrement :: State Tape ()
decrement = do
    cur <- gets current
    modify $ replace $ cur - 1

left :: State Tape ()
left = modify $ jump (-1)

right :: State Tape ()
right = modify $ jump 1

eval :: Char -> State Tape ()
eval c = case c of
    '+' -> increment
    '-' -> decrement
    '<' -> left
    '>' -> right
    _ -> fail "Invalid character"

-- IO State manipulation

printOut :: StateT Tape IO ()
printOut = do
    x <- gets current
    liftIO $ print $ chr x

getInput :: StateT Tape IO ()
getInput = do
    x <- liftIO $ query "Enter a character: "
    modify $ replace $ (ord . head) x

evalIO :: Char -> StateT Tape IO ()
evalIO c = case c of
    ',' -> getInput
    '.' -> printOut
    _ -> modify $ execState $ eval c

-- Interpreting a string

interpret :: String -> StateT Tape IO ()
interpret code = do
    if length code == 0
        then
            return ()
        else do
            chain evalIO code
    where
        chain f = foldl (\acc x -> acc >> f x) (return ())

runInterpreter :: String -> Tape -> IO Tape
runInterpreter str = execStateT $ interpret str