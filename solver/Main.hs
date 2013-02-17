module Main where

import Data.Label
import Text.Parsec (parse)

import Crossword.Description
import Crossword.Puzzle

main :: IO ()
main =
  do f <- readFile "data/puzzle"
     let (Right descr) = parse description "" f
         pzl = initialize descr
         sol = solve pzl
     putStrLn (showBoard (get board sol))
