{-# LANGUAGE LambdaCase #-}

module Parse (
    Node (..), parseProgram
) where

import Control.Applicative hiding (many)
import Control.Monad    

newtype Parser from to = Parser {parse :: [from] -> [(to, [from])]}

instance Functor (Parser from) where
    fmap f (Parser fun) = Parser $ \toks -> [(f a, b) | (a, b) <- fun toks]

instance Applicative (Parser from) where
    pure a = Parser $ \toks -> [(a, toks)]
    (Parser fun1) <*> (Parser fun2) = Parser $ \toks -> [(f a, toks2) | (f, toks1) <- fun1 toks, (a, toks2) <- fun2 toks1]

instance Monad (Parser from) where
    return = pure 
    (Parser p) >>= f = Parser $ \inp -> concat [parse (f tree) inp' | (tree, inp') <- p inp]

instance Alternative (Parser from) where
    empty = Parser $ const []
    (Parser p) <|> (Parser q) = Parser $ \toks -> case p toks of
        [] -> q toks
        res -> res

instance MonadPlus (Parser from) where
    mzero = empty
    (Parser p) `mplus` (Parser q) = Parser $ \inp-> p inp ++ q inp

-- DATA TYPES

data Node = Plus Int | Minus Int | MoveLeft Int | MoveRight Int | Input | Output | Loop [Node]
    deriving (Show)

-- PARSING PRIMITIVES

parseSymbol :: Eq a => a -> Parser a a
parseSymbol x = Parser $ \case
    (t:toks) -> [(t, toks) | t == x]
    _ -> []

parseAs :: Eq a => a -> b -> Parser a b
parseAs x y = parseSymbol x >> return y

many, many1 :: Parser a b -> Parser a [b]
many p = many1 p <|> pure []
many1 p = do
    x <- p
    rest <- many p
    return $ x : rest

parseGroup :: Char -> (Int -> Node) -> Parser Char Node
parseGroup c f = fmap (f . length) $ many1 (parseSymbol c)

-- PARSING BRAINFUCK

parsePlus, parseMinus, parseLeft, parseRight, parseInput, parseOutput :: Parser Char Node
parsePlus = parseGroup '+' Plus
parseMinus = parseGroup '-' Minus
parseLeft = parseGroup '<' MoveLeft
parseRight = parseGroup '>' MoveRight
parseInput = parseAs ',' Input
parseOutput = parseAs '.' Output

parseLoop :: Parser Char Node
parseLoop = do
    parseSymbol '['
    list <- many1 parseNode -- TODO: Custom error when empty loop detected
    parseSymbol ']'
    return $ Loop list

parseNode :: Parser Char Node
parseNode = parsePlus <|> parseMinus <|> parseLeft <|> parseRight <|> parseInput <|> parseOutput <|> parseLoop

parseCode :: Parser Char [Node]
parseCode = many parseNode

-- EXPORTED FUNCTIONS

runParser :: Parser a b -> [a] -> Either String b
runParser (Parser fun) toks = case fun toks of
    [(r, [])] -> Right r
    _ -> Left "Parser couldn't consume entire stream"

alphabet :: [Char]
alphabet = "+-<>,.[]"

parseProgram :: String -> Either String [Node]
parseProgram = runParser parseCode . filter (`elem` alphabet)