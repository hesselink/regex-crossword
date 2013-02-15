module Crossword.FixedRegex where

import Data.Set (Set, (\\))
import Crossword.Token

import qualified Data.Set as Set

type FixedRegex = [Atom]

data Atom =
    Any
  | Literal Token
  | OneOf (Set Token)
  | NoneOf (Set Token)
  deriving (Eq, Ord, Show)

ppr :: FixedRegex -> String
ppr = concatMap pprAtom

pprAtom :: Atom -> String
pprAtom (Literal (Token c)) = [c]
pprAtom Any = "."
pprAtom (OneOf toks) = "[" ++ map unToken (Set.toList toks) ++ "]"
pprAtom (NoneOf toks) = "[^" ++ map unToken (Set.toList toks) ++ "]"

merges :: [FixedRegex] -> FixedRegex
merges = foldr1 merge

merge :: FixedRegex -> FixedRegex -> FixedRegex
merge = zipWith mergeAtom

mergeAtom :: Atom -> Atom -> Atom
mergeAtom a     b     | a > b = mergeAtom b a
mergeAtom Any   _     = Any
mergeAtom l@(Literal t1) (Literal t2) | t1 == t2  = l
                                      | otherwise = OneOf (Set.fromList [t1, t2])
mergeAtom (Literal t1) (OneOf ts)  = OneOf (Set.insert t1 ts)
mergeAtom (Literal t)  (NoneOf ts) = noneOf (Set.delete t ts)
mergeAtom (OneOf ts1) (OneOf ts2) = OneOf (Set.union ts1 ts2)
mergeAtom (OneOf ts1) (NoneOf ts2) = noneOf (ts2 \\ ts1)
mergeAtom (NoneOf ts1) (NoneOf ts2) = noneOf (Set.intersection ts1 ts2)

noneOf :: Set Token -> Atom
noneOf ts =
  case Set.size ts of
    0 -> Any
    _ -> NoneOf ts

addToken :: Eq a => a -> [a] -> [a]
addToken x [] = [x]
addToken x l@(x':xs) | x == x'   = l
                     | otherwise = x' : addToken x xs
