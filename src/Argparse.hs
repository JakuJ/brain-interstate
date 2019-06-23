{-# LANGUAGE LambdaCase #-}

module Argparse (
    argparse, Argument(..)
) where

import Data.Char (isAlpha, isAlphaNum, isSpace, isSeparator, isPunctuation, isDigit)
import Control.Monad (guard)
import Control.Applicative (many, some, optional, (<|>))

import System.FilePath (isValid)

import Parse (Parser (..))

data Argument = Flag String | Option String FilePath | Single FilePath
    deriving (Show, Eq)

satisfy :: (a -> Bool) -> Parser a a
satisfy pred = Parser $ \case
    (t:ts) -> [(t, ts) | pred t]
    _      -> []

consume :: Char -> Parser Char Char
consume c = satisfy (==c)

parseWhitespace :: Parser Char String
parseWhitespace = some $ satisfy isSpace

parseShort :: Parser Char String
parseShort = do
    consume '-'
    c <- satisfy isAlpha
    return [c]

parseLong :: Parser Char String
parseLong = do
    consume '-'
    consume '-'
    first <- satisfy isAlpha
    rest <- many $ satisfy $ \c -> isAlpha c || c == '-'
    tail <- many $ satisfy isDigit
    return $ first : rest ++ tail

parseFlag :: Parser Char Argument
parseFlag = Flag <$> (parseLong <|> parseShort)

parseBareFilepath :: Parser Char FilePath
parseBareFilepath = do
    first <- satisfy (not . (== '-'))
    rest <- many $ satisfy (not . isSpace)
    return $ first : rest

parseQuotedFilepath :: Parser Char FilePath
parseQuotedFilepath = do
    quote <- consume '\"' <|> consume '\''
    path <- some $ satisfy (not . (==quote))
    consume quote
    return path

parseFilepath :: Parser Char FilePath
parseFilepath = do
    path <- parseQuotedFilepath <|> parseBareFilepath
    guard $ isValid path
    return path

parseOption :: Parser Char Argument
parseOption = do
    flag <- parseLong <|> parseShort
    parseWhitespace
    path <- parseFilepath
    return $ Option flag path

parseSingle :: Parser Char Argument
parseSingle = do
    path <- parseFilepath
    return $ Single path

parseArgument :: Parser Char Argument
parseArgument = parseOption <|> parseFlag <|> parseSingle 

parseArgs :: Parser Char [Argument]
parseArgs = do
    first <- parseArgument
    rest <- many (parseWhitespace >> parseArgument)
    return $ first : rest

argparse :: String -> Either String [Argument]
argparse "" = Right []
argparse args = case parse parseArgs args of
        [(as, [])]  -> Right as
        [(_, rest)] -> Left $ "Argument parsing error at: " ++ rest
        [] -> Left "Unable to parse string"
        _ -> Left "Unknown parsing error"