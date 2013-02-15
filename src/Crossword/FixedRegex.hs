module Crossword.FixedRegex where

import Control.Monad
import Data.Set (Set, (\\))
import Crossword.Token

import qualified Data.Set as Set

type FixedRegex = [Atom]

data Atom =
    Any
  | Literal Token
  | OneOf (Set Token)
  | NoneOf (Set Token)
  deriving (Eq, Show)

ppr :: FixedRegex -> String
ppr = concatMap pprAtom

pprAtom :: Atom -> String
pprAtom (Literal (Token c)) = [c]
pprAtom Any = "."
pprAtom (OneOf toks) = "[" ++ map unToken (Set.toList toks) ++ "]"
pprAtom (NoneOf toks) = "[^" ++ map unToken (Set.toList toks) ++ "]"

merges :: [FixedRegex] -> FixedRegex
merges = foldr1 merge

intersects :: [FixedRegex] -> Maybe FixedRegex
intersects = foldM1 intersect

merge :: FixedRegex -> FixedRegex -> FixedRegex
merge = zipWith mergeAtom

intersect :: FixedRegex -> FixedRegex -> Maybe FixedRegex
intersect = zipWithM intersectAtom

mergeAtom :: Atom -> Atom -> Atom
mergeAtom Any            _            = Any
mergeAtom _              Any          = Any
mergeAtom l@(Literal t1) (Literal t2) | t1 == t2  = l
                                      | otherwise = OneOf (Set.fromList [t1, t2])
mergeAtom (Literal t)    (OneOf ts)   = OneOf (Set.insert t ts)
mergeAtom (Literal t)    (NoneOf ts)  = noneOf (Set.delete t ts)
mergeAtom (OneOf ts)     (Literal t)  = OneOf (Set.insert t ts)
mergeAtom (OneOf ts1)    (OneOf ts2)  = OneOf (Set.union ts1 ts2)
mergeAtom (OneOf ts1)    (NoneOf ts2) = noneOf (ts2 \\ ts1)
mergeAtom (NoneOf ts1)   (NoneOf ts2) = noneOf (Set.intersection ts1 ts2)
mergeAtom (NoneOf ts)    (Literal t)  = noneOf (Set.delete t ts)
mergeAtom (NoneOf ts2)   (OneOf ts1)  = noneOf (ts2 \\ ts1)

intersectAtom :: Atom -> Atom -> Maybe Atom
intersectAtom Any            a             = Just a
intersectAtom a              Any           = Just a
intersectAtom l@(Literal t1) (Literal t2) | t1 == t2  = Just l
                                          | otherwise = Nothing
intersectAtom l@(Literal t)  (OneOf ts)    = if t `Set.member` ts then Just l  else Nothing
intersectAtom l@(Literal t)  (NoneOf ts)   = if t `Set.member` ts then Nothing else Just l
intersectAtom (OneOf ts)     l@(Literal t) = if t `Set.member` ts then Just l  else Nothing
intersectAtom (OneOf ts1)    (OneOf ts2)   = oneOf (Set.intersection ts1 ts2)
intersectAtom (OneOf ts1)    (NoneOf ts2)  = oneOf (ts1 \\ ts2)
intersectAtom (NoneOf ts1)   (NoneOf ts2)  = Just (NoneOf (Set.union ts1 ts2))
intersectAtom (NoneOf ts)    l@(Literal t) = if t `Set.member` ts then Nothing else Just l
intersectAtom (NoneOf ts2)   (OneOf ts1)   = oneOf (ts1 \\ ts2)

oneOf :: Set Token -> Maybe Atom
oneOf ts =
  case Set.toList ts of
    []  -> Nothing
    [t] -> Just (Literal t)
    _   -> Just (OneOf ts)

noneOf :: Set Token -> Atom
noneOf ts =
  case Set.size ts of
    0 -> Any
    _ -> NoneOf ts

addToken :: Eq a => a -> [a] -> [a]
addToken x [] = [x]
addToken x l@(x':xs) | x == x'   = l
                     | otherwise = x' : addToken x xs

foldM1             :: (Monad m) => (a -> a -> m a) -> [a] -> m a
foldM1 _ []       = error "Empty list in foldM1."
foldM1 _ [x]      = return x
foldM1 f (x:y:xs) =  f x y >>= \fax -> foldM1 f (fax:xs)
