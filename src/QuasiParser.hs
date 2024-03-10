{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module QuasiParser where

import Control.Monad (void, (<=<))
import Control.Monad.Identity (Identity)
import Data.Char (digitToInt, isSpace)
import Data.Data (Data, Typeable)
import Data.Functor (($>))
import Data.List (foldl')
import Language.Haskell.TH (conE)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (tupleDataName)
import Text.Parsec hiding (Empty)
import Text.Parsec.Expr (
    Assoc (..),
    Operator (..),
    OperatorTable,
    buildExpressionParser,
 )
import Debug.Trace (traceShow)

data Format
    = Empty
    | Signed
    | Unsigned
    | Char
    | Newline
    | String
    | Literal !String
    | Group !Format
    | Many !Format
    | Some !Format
    | Option !Format !Format
    | SepBy !Format !Format
    | Follows !Format !Format
    deriving (Show, Typeable, Data)

makeParser :: Format -> TH.ExpQ
makeParser fmt2 = traceShow fmt2 [|parseEither ($(toExpr fmt2) <* eof)|]

toExpr :: Format -> TH.ExpQ
toExpr = \case
    Empty -> [|return ()|]
    Newline -> [|void newline|]
    String -> [|many1 (satisfy (not . isSpace))|]
    Unsigned -> [|unsigned|]
    Signed -> [|signed|]
    Char -> [|many1 alphaNum|]
    Literal str -> [|void (string str)|]
    Group fmt -> [|$(toExpr fmt)|]
    Many f ->
        if interesting f
            then [|many $(toExpr f)|]
            else [|void (many $(toExpr f))|]
    Some f ->
        if interesting f
            then [|many1 $(toExpr f)|]
            else [|void (many1 $(toExpr f))|]
    SepBy l r ->
        if interesting l
            then [|sepBy $(toExpr l) $(toExpr r)|]
            else [|void (sepBy $(toExpr l) $(toExpr r))|]
    Option l r -> [|Left <$> $(toExpr l) <|> Right <$> $(toExpr r)|]
    fmt@(Follows _ _) -> do
        let fmts = [(interesting x, toExpr x) | x <- flatten fmt []]
            n = foldl' (\acc (x, _) -> if x then acc + 1 else acc) 0 fmts
            tup = conE (tupleDataName n)
        case fmts of
            [] -> [|return ()|]
            ((ii, e) : es)
                | n == 0 -> foldl' ap0 e es
                | n == 1 -> foldl' ap1 e es
                | ii -> foldl' apN [|$tup <$> $e|] es
                | otherwise -> foldl' apN [|$tup <$ $e|] es
      where
        ap0 l (_, r) = [|$l $> $r|]
        ap1 l (i, r) = if i then [|$l *> $r|] else [|$l <* $r|]
        apN l (i, r) = if i then [|$l <*> $r|] else [|$l <* $r|]

interesting :: Format -> Bool
interesting = \case
    Empty -> False
    Literal{} -> False
    Newline -> False
    Signed -> True
    Unsigned -> True
    Char -> True
    String -> True
    Group fmt -> interesting fmt
    Many fmt -> interesting fmt
    Some fmt -> interesting fmt
    SepBy l _ -> interesting l
    Option l r -> interesting l || interesting r
    Follows l r -> interesting l || interesting r

decimal :: [Int] -> Int
decimal = foldl' (\acc -> ((10 * acc) +)) 0

unsigned :: Parser Int
unsigned = decimal <$> many1 (digitToInt <$> digit)

signed :: Parser Int
signed = do
    f <- option id (char '-' $> negate <|> char '+' $> id)
    f . decimal <$> many1 (digitToInt <$> digit)

flatten :: Format -> [Format] -> [Format]
flatten (Literal x) (Literal y : ys) = flatten (Literal (x ++ y)) ys
flatten Empty xs = xs
flatten (Follows l r) xs = flatten l (flatten r xs)
flatten x ys = x : ys

parseF :: String -> TH.Q Format
parseF s = case parseEither (factor1 <* eof) s of
    Left err -> fail (show err)
    Right r -> return r

toType :: Format -> TH.TypeQ
toType = \case
    Empty -> [t|()|]
    Newline -> [t|()|]
    Unsigned -> [t|Int|]
    Signed -> [t|Int|]
    String -> [t|String|]
    Char -> [t|Char|]
    Literal _ -> [t|()|]
    Group fo -> [t|$(toType fo)|]
    Many fo -> [t|[$(toType fo)]|]
    Some fo -> [t|[$(toType fo)]|]
    SepBy l _ -> [t|$(toType l)|]
    Option l r -> [t|Either $(toType l) $(toType r)|]
    Follows l r -> [t|($(toType l), $(toType r))|]

format :: QuasiQuoter
format =
    QuasiQuoter
        { quoteExp = makeParser <=< parseF
        , quoteType = toType <=< parseF
        }

-- Parsing regex

type Parser = Parsec String ()
type Table a = OperatorTable String () Identity a

parseEither :: Parser a -> String -> Either ParseError a
parseEither p = parse (p <* eof) ""

factor1 :: Parser Format
factor1 =
    choice
        [ try $ factor2 `chainl1` (char '|' $> Option)
        , factor2
        ]
factor2 :: Parser Format
factor2 = choice [try $ factor3 `chainl1` (return () $> Follows), factor3]

factor3 :: Parser Format
factor3 =
    choice
        [ try $ buildExpressionParser table atom
        , atom
        ]

atom :: Parser Format
atom =
    choice
        [ try $ Group <$> between (char '(') (char ')') factor1
        , try $ string "%s" $> String
        , try $ string "%d" $> Signed
        , try $ string "%c" $> Char
        , try $ string "%n" $> Newline
        , try $ string "%u" $> Unsigned
        , try $ Literal <$> (char '\\' *> fmap (:[]) chars)
        , Literal <$> many1 chars
        ]
chars :: Parser Char
chars = noneOf "%()\\*+&"

table :: Table Format
table =
    [
        [ Postfix $ char '*' $> Many
        , Postfix $ char '+' $> Some
        , Infix (char '&' $> SepBy) AssocLeft
        ]
    ]
