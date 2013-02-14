module Crossword.Regex where

import Crossword.Token

data Regex =
    Literal Token
  | Any
  | OneOf [Token]
  | NoneOf [Token]
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
