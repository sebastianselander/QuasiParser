{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.List (foldl')

data Pos = Pos {row :: !Int, col :: !Int}

-- TODO: Have either be outer most as we lose the position otherwise.
newtype Parser a = Parser {runParser :: String -> StateT Pos (Either String) (a, String)}

parseEither :: Parser a -> String -> Either String a
parseEither p s = fst <$> evalStateT (runParser p s) (Pos 0 0)

parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s = case fst <$> evalStateT (runParser p s) (Pos 0 0) of
    Left _ -> Nothing
    Right x -> Just x

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser g) = Parser $ fmap (first f) . g

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (\s -> return (x, s))
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser f <*> Parser g = Parser $ \s -> do
        (h, s') <- f s
        s'' <- g s'
        return (first h s'')

instance Monad Parser where
    return :: a -> Parser a
    return = pure
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser a >>= f = Parser $ \s -> do
        (a', s') <- a s
        runParser (f a') s'

instance MonadFail Parser where
    fail :: String -> Parser a
    fail s = Parser (const (throwError s))

instance Alternative Parser where
    (<|>) :: Parser a -> Parser a -> Parser a
    Parser l <|> Parser r = Parser $ \s -> catchError (l s) (\_ -> r s)

    empty :: Parser a
    empty = Parser $ const (throwError empty)

    many p = some p <|> pure []
    some p = (:) <$> p <*> many p

addCol :: (Monad m) => Int -> StateT Pos m ()
addCol n = do
    (Pos row col) <- get
    put (Pos row (col + n))

addRow :: (Monad m) => Int -> StateT Pos m ()
addRow n = do
    (Pos row col) <- get
    put (Pos (row + n) col)

char :: Char -> Parser Char
char '\n' =
    Parser
        ( \case
            [] -> throwError "Expected '\\n', but got <empty input>"
            (x : xs)
                | x == '\n' -> addRow 1 >> return (x, xs)
                | otherwise -> throwError ("Expected '\\n',  but got '" <> [x] <> "'")
        )
char c = Parser $ \case
    [] -> throwError ("Expected '" <> [c] <> "', but got <empty input>")
    (x : xs)
        | x == c -> addCol 1 >> return (x, xs)
        | otherwise -> throwError ("Expected '" <> [c] <> "', but got '" <> [x] <> "'")

string :: String -> Parser String
string = mapM char

digit :: Parser Int
digit =
    foldl'
        (<|>)
        empty
        [ char '0' $> 0
        , char '1' $> 1
        , char '2' $> 2
        , char '3' $> 3
        , char '4' $> 4
        , char '5' $> 5
        , char '6' $> 6
        , char '7' $> 7
        , char '8' $> 8
        , char '9' $> 9
        ]

d :: Parser Int
d = decimal <$> some digit

decimal :: (Num a) => [a] -> a
decimal = foldl' (\acc x -> acc * 10 + x) 0

u :: Parser Int
u = do
    sign <- char '-' <|> char '+'
    let f = case sign of
            '-' -> (* (-1))
            _ -> id
    f <$> d
