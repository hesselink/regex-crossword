module Crossword.FixedRegex where

import Control.Monad
import Data.Maybe
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
                                      | otherwise = Just (OneOf (Set.fromList [t1, t2]))
mergeAtom (Literal t1) (OneOf ts)  = Just (OneOf (Set.insert t1 ts))
mergeAtom (Literal t)  n@(NoneOf ts) | t `Set.member` ts = Nothing
                                     | otherwise = Just n
mergeAtom (OneOf ts1) (OneOf ts2) = Just (OneOf (Set.union ts1 ts2))
mergeAtom (OneOf ts1) (NoneOf ts2) =
  let ts3 = ts1 \\ ts2
  in case Set.size ts3 of
       0 -> Nothing
       1 -> Just (Literal (head (Set.toList ts3)))
       _ -> Just (OneOf ts3)
mergeAtom (NoneOf ts1) (NoneOf ts2) = Just (OneOf (Set.union ts1 ts2))

addToken :: Eq a => a -> [a] -> [a]
addToken x [] = [x]
addToken x l@(x':xs) | x == x'   = l
                     | otherwise = x' : addToken x xs
