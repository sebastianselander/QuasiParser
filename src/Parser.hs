{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.List (foldl')

data Pos = Pos {row :: !Int, col :: !Int}
type Error = String

instance Show Pos where
    show (Pos row col) = "line: " <> show row <> ", column: " <> show col

startPos :: Pos
startPos = Pos 1 1

posToIdx :: Pos -> Int
posToIdx (Pos a b) = a + b - 2

newtype Parser a = Parser {runParser :: String -> ExceptT Error (State Pos) (a, String)}

-- TODO: Make safer
getChr :: Pos -> String -> Char
getChr (Pos row col) s =
    let xs = lines s
     in xs !! (row - 1) !! (col - 1)

parseEither :: Parser a -> String -> Either String a
parseEither p s =
    let (a, pos) = runState (runExceptT (runParser p s)) startPos
     in case a of
            Left s' ->
                Left
                    ( "Expected '"
                        <> s'
                        <> "', but got '"
                        <> [getChr pos s]
                        <> "', at "
                        <> show pos
                    )
            Right (x, _) -> Right x

parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s = case evalState (runExceptT (runParser p s)) startPos of
    Left _ -> Nothing
    Right (a, _) -> Just a

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

modifyError :: (Error -> Error) -> Parser a -> Parser a
modifyError f (Parser p) = Parser (withExceptT f . p)

resetCol :: ExceptT error (State Pos) ()
resetCol = do
    Pos row _ <- get
    put (Pos row 1)

addCol :: Int -> ExceptT error (State Pos) ()
addCol n = do
    Pos row col <- get
    put (Pos row (col + n))

addRow :: Int -> ExceptT error (State Pos) ()
addRow n = do
    Pos row col <- get
    put (Pos (row + n) col)

char :: Char -> Parser Char
char '\n' =
    Parser
        ( \case
            [] -> throwError "\\n"
            (x : xs)
                | x == '\n' -> resetCol >> addRow 1 >> return (x, xs)
                | otherwise -> throwError "\\n"
        )
char c = Parser $ \case
    [] -> throwError [c]
    (x : xs)
        | x == c -> addCol 1 >> return (x, xs)
        | otherwise -> throwError [c]

string :: String -> Parser String
string = mapM char

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

eol :: Parser ()
eol = void (char '\n')

look :: Parser String
look = Parser $ \s -> return (s, s)

consume :: Parser Char
consume = Parser $ \case
    [] -> throwError "<something>"
    (x : xs) -> return (x, xs)

eof :: Parser ()
eof = do
    s <- look
    unless (null s) (fail "<eof>")

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    s <- consume
    if f s
        then return s
        else fail "<failed predicate>"

choice :: [Parser a] -> Parser a
choice = foldl' (<|>) empty

digit :: Parser Int
digit =
    modifyError (const "<digit>") $
        choice
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
chainl1 :: Parser b -> Parser (b -> b -> b) -> Parser b
chainl1 p op = do
    x <- p
    rest x
  where
    rest x =
        ( do
            f <- op
            y <- p
            rest (f x y)
        )
            <|> return x

decimal :: Parser Int
decimal = go <$> some digit
  where
    go :: (Num a) => [a] -> a
    go = foldl' (\acc x -> acc * 10 + x) 0

signed :: Parser Int
signed = do
    sign <- char '-' $> negate <|> char '+' $> id
    sign <$> decimal
