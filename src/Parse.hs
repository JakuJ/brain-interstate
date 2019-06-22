{-# LANGUAGE LambdaCase #-}

module Parse (
    Node (..), parseProgram
) where

import Control.Applicative
import Control.Monad.Except

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

parseGroup :: Char -> (Int -> Node) -> Parser Char Node
parseGroup c f = fmap (f . length) $ some (parseSymbol c)

-- PARSING BRAINFUCK

parsePlus, parseMinus, parseLeft, parseRight, parseInput, parseOutput :: Parser Char Node
parsePlus = parseGroup '+' Plus
parseMinus = parseGroup '-' Minus
parseLeft = parseGroup '<' MoveLeft
parseRight = parseGroup '>' MoveRight
parseInput = parseAs ',' Input
parseOutput = parseAs '.' Output

parseSingletons :: Parser Char Node
parseSingletons = parsePlus <|> parseMinus <|> parseLeft <|> parseRight <|> parseInput <|> parseOutput

parseLoop :: ExceptT String (Parser Char) Node
parseLoop = do
    lift $ parseSymbol '['
    list <- lift parseCode
    rb <- lift $ optional $ parseSymbol ']'

    case rb of
        Nothing -> throwError "Missing right bracket"
        Just ']' -> case list of
            Left err -> throwError err    
            Right [] -> throwError "Infinite loop detected"
            Right ls -> return $ Loop ls

parseCharacter :: Parser Char (Either String Node)
parseCharacter = runExceptT parseLoop <|> (Right <$> parseSingletons)

parseCode :: Parser Char (Either String [Node])
parseCode = (foldM chain []) <$> (many parseCharacter)
    where
        chain xs (Right x) = Right $ xs ++ [x]
        chain xs (Left err) = Left err

-- EXPORTED FUNCTIONS

runParser :: Parser a (Either String b) -> [a] -> Either String b
runParser (Parser fun) toks = case fun toks of
    [(Left err, _)] -> Left err
    [(Right r, [])] -> Right r
    [(Right _, _)]  -> Left "Missing left bracket"
    _               -> Left "Unknown parsing error"

alphabet :: [Char]
alphabet = "+-<>,.[]"

parseProgram :: String -> Either String [Node]
parseProgram = runParser parseCode . filter (`elem` alphabet)