module Crossword.Regex where

import Data.Set (Set)

import qualified Data.Set as Set

import Crossword.Token

data Regex =
    Literal Token
  | Any
  | OneOf (Set Token)
  | NoneOf (Set Token)
  | Many Regex
  | Many1 Regex
  | Seq Regex Regex
  | Group Regex
  | BackRef Int
  | Choice Regex Regex
  | Option Regex
  deriving Show

lit :: Char -> Regex
lit = Literal . Token

ppr :: Regex -> String
ppr (Literal (Token c)) = [c]
ppr Any = "."
ppr (OneOf toks) = "[" ++ map unToken (Set.toList toks) ++ "]"
ppr (NoneOf toks) = "[^" ++ map unToken (Set.toList toks) ++ "]"
ppr (Many r) = ppr r ++ "*"
ppr (Many1 r) = ppr r ++ "+"
ppr (Seq r1 r2) = ppr r1 ++ ppr r2
ppr (Group r) = "(" ++ ppr r ++ ")"
ppr (BackRef i) = "\\" ++ show i
ppr (Choice r1 r2) = ppr r1 ++ "|" ++ ppr r2
ppr (Option r) = ppr r ++ "?"

seq, choices :: [Regex] -> Regex
seq = foldr1 Seq

choices = foldr1 Choice
