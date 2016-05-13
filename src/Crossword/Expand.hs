{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crossword.Expand where

import Control.Arrow
import Control.Monad
import Control.Monad.State (StateT (..))
import Data.IntMap (IntMap)
import Data.Label
import Data.Label.Monadic
import Data.Maybe
import Data.Monoid ((<>))

import qualified Data.IntMap as Map

import Crossword.FixedRegex (FixedRegex, Atom)
import Crossword.GenState
import Crossword.Regex
import qualified Crossword.FixedRegex as F

expand :: FixedRegex -> Regex -> [FixedRegex]
expand pat r = filter ((== (length pat)) . length)
             . map (uncurry replaceGroups . second (get groups))
             $ runStateT (expand' r) (emptyState pat)

data Item = Item Atom | Ref Int

replaceGroups :: [Item] -> IntMap [Item] -> FixedRegex
replaceGroups is grps = concatMap replaceSingle is
  where
    replaceSingle (Item a) = [a]
    replaceSingle (Ref  i) = replaceGroups (fromJust (Map.lookup i grps)) grps

expand' :: Regex -> StateT (GenState FixedRegex [Item]) [] [Item]
expand' (Literal t) = fixed (F.Literal t)
expand' Any     {}  = fixed F.Any
expand' (OneOf ts)  = fixed (F.OneOf ts)
expand' (NoneOf ts) = fixed (F.NoneOf ts)
expand' (Many r)    = return mempty `mplus` expand' (Many1 r)
expand' (Many1 r)   =
  do x  <- expand' r
     xs <- expand' (Many r)
     return (x <> xs)
expand' (Seq r1 r2)   =
  do x <- expand' r1
     y <- expand' r2
     return (x <> y)
expand' (Group r)   =
  do x <- expand' r
     i <- storeGroup x
     return [Ref i]
expand' (BackRef i)   =
  do g  <- getGroup i
     as <- replicateM (length g) popAtom
     let mr = intersectItems (map Item as) g
     case mr of
       Nothing -> mzero
       Just r  ->
         do updateGroup i r
            return [Ref i]
expand' (Choice r1 r2) = expand' r1 `mplus` expand' r2
expand' (Option r)    = return mempty `mplus` expand' r

intersectItems :: [Item] -> [Item] -> Maybe [Item]
intersectItems = zipWithM intersectItem

intersectItem :: Item -> Item -> Maybe Item
intersectItem (Item a1) (Item a2) = Item <$> F.intersectAtom a1 a2
intersectItem (Item a ) (Ref  _ ) = Just (Item a)
intersectItem (Ref  _ ) (Item a ) = Just (Item a)
intersectItem (Ref  i1) (Ref  i2) | i1 == i2  = Just (Ref i1)
                                  | otherwise = Just (Ref i1) -- TODO: this is wrong

fixed :: Atom -> StateT (GenState FixedRegex a) [] [Item]
fixed r =
  do a <- popAtom
     let ma = F.intersectAtom a r
     maybe mzero (return . (:[]) . Item) ma

popAtom :: StateT (GenState FixedRegex a) [] Atom
popAtom =
  do as <- gets state
     case as of
       []      -> mzero
       (a:as') ->
         do puts state as'
            return a
