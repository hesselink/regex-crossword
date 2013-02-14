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

ppr :: Regex -> String
ppr (Literal (Token c)) = [c]
ppr Any = "."
ppr (OneOf toks) = "[" ++ map unToken toks ++ "]"
ppr (NoneOf toks) = "[^" ++ map unToken toks ++ "]"
ppr (Many r) = ppr r ++ "*"
ppr (Many1 r) = ppr r ++ "+"
ppr (Seq r1 r2) = ppr r1 ++ ppr r2
ppr (Group r) = "(" ++ ppr r ++ ")"
ppr (BackRef i) = "\\" ++ show i
ppr (Choice r1 r2) = ppr r1 ++ "|" ++ ppr r2
ppr (Option r) = ppr r ++ "?"
