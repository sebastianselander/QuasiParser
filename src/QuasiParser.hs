{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module QuasiParser where

import Control.Applicative
import Control.Monad (void)
import Data.Char (isAlpha)
import Data.Data (Data, Typeable)
import Data.Functor (($>))
import Data.List (foldl')
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote
import Parser

data Format
    = Decimal
    | Char
    | Literal !String
    | Many !Format
    | Some !Format
    | Option !Format !Format
    deriving (Show, Typeable, Data)

interpret :: Format -> TH.ExpQ
interpret = \case
    Literal str -> [|void (string str)|]
    Decimal -> [|decimal|]
    Many f -> [|many $(interpret f)|]
    Some f -> [|some $(interpret f)|]
    Char -> [|satisfy isAlpha|]
    Option l r -> [|$(interpret l) <|> $(interpret r)|]

quasiFormat :: String -> TH.ExpQ
quasiFormat s = case parseEither start s of
    Left s' -> fail s'
    Right x -> [|parseEither $(interpret x)|]

format :: QuasiQuoter
format = QuasiQuoter{quoteExp = quasiFormat}

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
start = regex `chainl1` (char '|' $> Option)

optionP :: Parser Format
optionP = atom `chainl1` (char '|' $> Option)
