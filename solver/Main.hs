module Main where

import Control.Arrow
import Text.Parsec (parse)

import Crossword.Expand
import Crossword.FixedRegex
import Crossword.Parser

main = readFile "data/puzzle" >>=
    print
  . map ( ppr
        . merges
        . uncurry expand
        . (read *** (either (error . show) id . parse regex ""))
        . (\[x,y] -> (x,y))
        . words
        )
  . filter (/= "")
  . lines
