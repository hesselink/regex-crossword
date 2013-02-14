module Crossword.FixedRegex where

import Control.Monad
import Data.List ((\\))
import Data.Maybe
import Crossword.Token

type FixedRegex = [Atom]

data Atom =
    Any
  | Literal Token
  | OneOf [Token]
  | NoneOf [Token]
  deriving (Eq, Ord, Show)

ppr :: FixedRegex -> String
ppr = concatMap pprAtom

pprAtom :: Atom -> String
pprAtom (Literal (Token c)) = [c]
pprAtom Any = "."
pprAtom (OneOf toks) = "[" ++ map unToken toks ++ "]"
pprAtom (NoneOf toks) = "[^" ++ map unToken toks ++ "]"

merges :: [FixedRegex] -> [FixedRegex]
merges [] = []
merges [x] = [x]
merges [x,y] = maybeToList (merge x y)
merges (x:y:xs) = merges (maybeToList (merge x y) ++ xs)

merge :: FixedRegex -> FixedRegex -> Maybe FixedRegex
merge = zipWithM mergeAtom

mergeAtom :: Atom -> Atom -> Maybe Atom
mergeAtom a     b     | a > b = mergeAtom b a
mergeAtom Any   _     = Just Any
mergeAtom l@(Literal t1) (Literal t2) | t1 == t2  = Just l
                                      | otherwise = Just (OneOf [t1, t2])
mergeAtom (Literal t1) (OneOf ts)  = Just (OneOf (addToken t1 ts))
mergeAtom (Literal t)  n@(NoneOf ts) | t `elem` ts = Nothing
                                     | otherwise = Just n
mergeAtom (OneOf ts1) (OneOf ts2) = Just (OneOf (foldr addToken ts1 ts2))
mergeAtom (OneOf ts1) (NoneOf ts2) =
  case ts1 \\ ts2 of
    []  -> Nothing
    [t] -> Just (Literal t)
    ts  -> Just (OneOf ts)
mergeAtom (NoneOf ts1) (NoneOf ts2) = Just (NoneOf (foldr addToken ts1 ts2))

addToken :: Eq a => a -> [a] -> [a]
addToken x [] = [x]
addToken x l@(x':xs) | x == x'   = l
                     | otherwise = x' : addToken x xs
