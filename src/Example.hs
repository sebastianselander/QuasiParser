{-# LANGUAGE QuasiQuotes #-}
module Example where

import QuasiParser

test = do
    res <- [format|(Game %d: %d&((green|red|blue), )&(; ))*|] "Game 98: 7 green, 4 blue, 1 red; 2 red, 5 blue; 4 blue, 3 red, 7 green"
    Right res
