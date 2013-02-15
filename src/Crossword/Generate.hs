{-# LANGUAGE TupleSections #-}
module Crossword.Generate where

import Control.Monad.State (StateT (..))
import Control.Monad
import Data.Label.PureM
import Data.Set ((\\))

import qualified Data.Set    as Set

import Crossword.GenState
import Crossword.Regex
import Crossword.Token

generate :: Int -> Regex -> [String]
generate l r = filter ((== l) . length)
             . map fst
             $ runStateT (generate' r) (emptyState l)

generate' :: Regex -> StateT (GenState String) [] String
generate' (Literal t) = genToken t
generate' Any =
  do l <- gets availableLength
     guard (l > 0)
     x <- list enumAll
     genTokenUnsafe x
generate' (OneOf toks) = set toks >>= genToken
generate' (NoneOf toks) = set (Set.fromList enumAll \\ toks) >>= genToken
generate' (Many r) = return "" `mplus`
  do x <- generate' r
     xs <- generate' (Many r)
     return (x ++ xs)
generate' (Many1 r) =
  do x <- generate' r
     xs <- generate' (Many r)
     return (x ++ xs)
generate' (Seq r1 r2) =
  do x <- generate' r1
     y <- generate' r2
     return (x ++ y)
generate' (Group r) =
  do x <- generate' r
     storeGroup x
     return x
generate' (BackRef i) =
  do l <- gets availableLength
     x <- getGroup i
     guard (l >= length x)
     modify availableLength (subtract (length x))
     return x
generate' (Choice r1 r2) = generate' r1 `mplus` generate' r2
generate' (Option r) = return "" `mplus` generate' r

genToken :: Token -> StateT (GenState String) [] String
genToken (Token c) =
  do l <- gets availableLength
     guard (l > 0)
     puts availableLength (l - 1)
     return [c]

genTokenUnsafe :: Token -> StateT (GenState String) [] String
genTokenUnsafe (Token c) =
  do modify availableLength (subtract 1)
     return [c]
