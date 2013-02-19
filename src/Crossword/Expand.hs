{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crossword.Expand where

import Control.Monad
import Control.Monad.State (StateT (..))
import Data.Label.PureM
import Data.Monoid (Monoid (..), (<>))

import Crossword.FixedRegex (FixedRegex, Atom)
import Crossword.GenState
import Crossword.Regex
import qualified Crossword.FixedRegex as F

expand :: FixedRegex -> Regex -> [FixedRegex]
expand pat r = filter ((== (length pat)) . length)
             . map fst
             $ runStateT (expand' r) (emptyState pat)

expand' :: Regex -> StateT (GenState FixedRegex FixedRegex) [] FixedRegex
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
     storeGroup x
     return x
-- TODO: this doesn't propagate information backwards: if you have
-- somelike like '.\1', and pass in '.A' as a pattern, it will
-- generate '.A', not 'AA'.
expand' (BackRef i)   =
  do g  <- getGroup i
     as <- replicateM (length g) popAtom
     let mr = F.intersect as g
     case mr of
       Nothing -> mzero
       Just r  ->
         do updateGroup i r
            return r

expand' (Choice r1 r2) = expand' r1 `mplus` expand' r2
expand' (Option r)    = return mempty `mplus` expand' r

fixed :: Atom -> StateT (GenState FixedRegex a) [] FixedRegex
fixed r =
  do a <- popAtom
     let ma = F.intersectAtom a r
     maybe mzero (return . (:[])) ma

popAtom :: StateT (GenState FixedRegex a) [] Atom
popAtom =
  do as <- gets state
     case as of
       []      -> mzero
       (a:as') ->
         do puts state as'
            return a
