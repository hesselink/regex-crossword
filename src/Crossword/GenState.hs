{-# LANGUAGE TupleSections, TemplateHaskell #-}
module Crossword.GenState where

import Control.Monad.State (StateT (..))
import Data.IntMap (IntMap)
import Data.Label (mkLabel)
import Data.Label.Monadic
import Data.Maybe (maybeToList)
import Data.Set (Set)

import qualified Data.IntMap as M
import qualified Data.Set    as Set

data GenState s a = GenState
  { _state :: s
  , _nextGroup :: !Int
  , _groups :: IntMap a
  }

mkLabel ''GenState

emptyState :: s -> GenState s a
emptyState s = GenState s 1 M.empty

storeGroup :: a -> StateT (GenState s a) [] Int
storeGroup s =
  do g <- gets nextGroup
     modify nextGroup (+1)
     modify groups (M.insert g s)
     return g

updateGroup :: Int -> a -> StateT (GenState s a) [] ()
updateGroup g s = modify groups (M.insert g s)

getGroup :: Int -> StateT (GenState s a) [] a
getGroup g =
  do gs <- gets groups
     list (maybeToList (M.lookup g gs))

list :: [a] -> StateT s [] a
list xs = StateT (\s -> map (,s) xs)

set :: Set a -> StateT s [] a
set xs = StateT (\s -> map (,s) (Set.toList xs))
