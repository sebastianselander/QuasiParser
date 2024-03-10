{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module QuasiParser where

import Control.Monad (void, (<=<))
import Data.Char (digitToInt, isSpace)
import Data.Data (Data, Typeable)
import Data.Functor (($>))
import Data.List (foldl')
import Debug.Trace (trace)
import Language.Haskell.TH (conE)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (tupleDataName)
import Text.Parsec hiding (Empty)
import Control.Applicative ((<**>))

data Format
    = Empty
    | Decimal
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
makeParser fmt2 = trace (show fmt2) [|parseEither ($(toExpr fmt2) <* eof)|]

toExpr :: Format -> TH.ExpQ
toExpr = \case
    Empty -> [|return ()|]
    Newline -> [|void newline|]
    String -> [|many1 (satisfy (not . isSpace))|]
    Decimal -> [|decimal|]
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
                | otherwise -> foldl' apN [|$tup $> $e|] es
      where
        ap0 l (_, r) = [|$l <$ $r|]
        ap1 l (i, r) = if i then [|$l *> $r|] else [|$l <* $r|]
        apN l (i, r) = if i then [|$l <*> $r|] else [|$l <* $r|]

interesting :: Format -> Bool
interesting = \case
    Empty -> False
    Literal{} -> False
    Newline -> False
    Decimal -> True
    Char -> True
    String -> True
    Group fmt -> interesting fmt
    Many fmt -> interesting fmt
    Some fmt -> interesting fmt
    SepBy l _ -> interesting l
    Option l r -> interesting l || interesting r
    Follows l r -> interesting l || interesting r

decimal :: Parser Int
decimal = foldl' (\acc -> ((10 * acc) +)) 0 <$> many1 (digitToInt <$> digit)

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
    Decimal -> [t|Int|]
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

postfix :: (a -> b) -> Parser a -> Parser (b -> b) -> Parser b
postfix wrap p op = (wrap <$> p) <**> rest
    where rest = flip (.) <$> op <*> rest <|> return id

type Parser = Parsec String ()

parseEither :: Parser a -> String -> Either ParseError a
parseEither p = parse (p <* eof) ""

factor4 :: Parser Format
factor4 =
    choice
        [ try $ string "%d" $> Decimal
        , try $ string "%c" $> Char
        , try $ string "%n" $> Newline
        , try $ string "%s" $> String
        , -- TODO: Make literal be anything but key characters
          try $ Literal <$> (many1 alphaNum <|> string "\\n" <|> many1 (char ' '))
        , Group <$> between (char '(') (char ')') factor1
        ]

factor3 :: Parser Format
factor3 =
    choice
        [ try $ postfix id factor4 (Many <$ char '*')
        , try $ postfix id factor4 (Some <$ char '+')
        , try $ factor4 `chainl1` (char '&' $> SepBy)
        , factor4
        ]

factor2 :: Parser Format
factor2 =
    choice
        [ try $ factor4 `chainr1` (return () $> Follows)
        , factor3
        ]

factor1 :: Parser Format
factor1 =
    choice
        [ try $ factor2 `chainl1` (char '|' $> Option)
        , factor2
        ]
