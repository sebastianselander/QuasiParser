{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module QuasiParser where

import Control.Applicative
import Control.Monad (void, (<=<))
import Data.Char (isAlpha)
import Data.Data (Data, Typeable)
import Data.Functor (($>))
import Data.List (foldl')
import Debug.Trace (trace, traceShow)
import GHC.IO (liftIO)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote
import Parser

data Format
    = Empty
    | Decimal
    | Char
    | Literal !String
    | Many !Format
    | Some !Format
    | Option !Format !Format
    | Follows !Format !Format
    deriving (Show, Typeable, Data)

quasiFormat :: String -> TH.ExpQ
quasiFormat s = case parseEither start s of
    Left s' -> fail s'
    Right x -> trace (show x) [|parseEither ($(toExpr x) <* eof)|]

toExpr :: Format -> TH.ExpQ
toExpr = \case
    Literal str -> [|void (string str)|]
    Decimal -> [|decimal|]
    Many f -> [|many $(toExpr f)|]
    Some f -> [|some $(toExpr f)|]
    Char -> [|satisfy isAlpha|]
    Option l r -> [|Left <$> $(toExpr l) <|> Right <$> $(toExpr r)|]
    Follows l r -> undefined

parse :: String -> TH.Q Format
parse s = case parseEither start s of
    Left s' -> fail s'
    Right fmt -> return fmt

toType :: Format -> TH.TypeQ
toType = \case
    Decimal -> [t|Int|]
    Char -> [t|Char|]
    Literal _ -> [t|()|]
    Many fo -> [t|$(toType fo)|]
    Some fo -> [t|$(toType fo)|]
    Option l r -> [t|Either $(toType l) $(toType r)|]
    Follows l r -> undefined

format :: QuasiQuoter
format = QuasiQuoter{quoteExp = quasiFormat, quoteType = toType <=< parse}

-- Parsing regex

atom :: Parser Format
atom =
    choice
        [ string "%d" $> Decimal
        , string "%c" $> Char
        , Literal <$> some (satisfy isAlpha)
        ]

regex :: Parser Format
regex =
    choice
        [ Many <$> atom <* char '*'
        , Some <$> atom <* char '+'
        , atom
        ]

start :: Parser Format
start =
    choice
        [ 
        try (Follows <$> regex <*> start)
        , try (regex `chainl1` (char '|' $> Option))
        , pSuccess $> Empty
        ]

optionP :: Parser Format
optionP = atom `chainl1` (char '|' $> Option)
