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

expand :: Int -> Regex -> [FixedRegex]
expand l r = filter ((== l) . length)
             . map fst
             $ runStateT (expand' r) (emptyState l)

expand' :: Regex -> StateT (GenState FixedRegex) [] FixedRegex
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
expand' (BackRef i)   =
  do l <- gets availableLength
     x <- getGroup i
     guard (l >= length x)
     modify availableLength (subtract (length x))
     return x
expand' (Choice r1 r2) = expand' r1 `mplus` expand' r2
expand' (Option r)    = return mempty `mplus` expand' r

fixed :: Atom -> StateT (GenState a) [] FixedRegex
fixed r =
  do l <- gets availableLength
     guard (l > 0)
     puts availableLength (l - 1)
     return [r]
