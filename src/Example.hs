{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Example where

import QuasiParser

data Color = Red | Green | Blue
    deriving (Show, Eq)

data Colour = Colour_red | Colour_green | Colour_blue
    deriving (Show, Eq)

intro

example1 :: Bool
example1 = [123, 892] == [fmt|%d&%n|] "123\n892"

example2 :: Bool
example2 = [fmt|@Color|] "Red" == Red

example3 :: Bool
example3 = [fmt|@Colour|] "red" == Colour_red

example4 :: Bool
example4 = [fmt|(%d!&%n)|] "123\n123" == ["123", "123"]

example5 :: Bool
example5 = [fmt|(%d!&%n)!|] "123\n123" == "123\n123"

aoc4 :: String
aoc4 = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n"
