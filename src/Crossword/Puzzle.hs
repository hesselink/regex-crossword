{-# LANGUAGE TupleSections, TemplateHaskell, TypeOperators #-}
module Crossword.Puzzle where

import Control.Arrow
import Data.Foldable (Foldable, foldMap)
import Data.Function
import Data.List (intercalate, groupBy, sortBy)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid (Endo (..))
import Data.Ord
import Data.Label
import Data.Tuple

import qualified Data.Map as Map

import Crossword.Description (Description, Position, Length, Clue)
import Crossword.Expand
import Crossword.FixedRegex
import Crossword.Regex (Regex)

import qualified Crossword.Description as D

type Board = Map Position Atom

data Puzzle = Puzzle
  { _board      :: Board
  , _horizontal :: [Clue Regex]
  , _vertical   :: [Clue Regex]
  , _diagonal   :: [Clue Regex]
  } deriving Show

mkLabel ''Puzzle

initialize :: Description -> Puzzle
initialize descr =
  let setRowToAny :: [Position] -> Board -> Board
      setRowToAny = foldMapEndo (flip Map.insert Any)
      horizontalRows = map (uncurry (row Horizontal) . (D.position &&& D.len)) . D.horizontal
      initialBoard = (foldMapEndo setRowToAny . horizontalRows $ descr) Map.empty
  in Puzzle initialBoard (D.horizontal descr) (D.vertical descr) (D.diagonal descr)

solve :: Puzzle -> Puzzle
solve p =
  let p' = step p
  in if get board p == get board p'
     then p'
     else solve p'

step :: Puzzle -> Puzzle
step = stepClues Diagonal
     . stepClues Vertical
     . stepClues Horizontal

data Direction = Horizontal | Vertical | Diagonal

stepClues :: Direction -> Puzzle -> Puzzle
stepClues dir pzl = foldMapEndo (stepClue dir) (get (label dir) pzl) pzl

stepClue :: Direction -> Clue Regex -> Puzzle -> Puzzle
stepClue dir c pzl = modify board updateBoard
                   $ pzl
  where
    updateBoard = foldMapEndo (uncurry (Map.insertWith (\x -> fromJust . intersectAtom x))) $ zip poss newRow
    newRow = merges newOpts
    newOpts = expand curRow (D.clue c)
    poss = row dir (D.position c) (D.len c)
    curRow = map (fromJust . flip Map.lookup (get board pzl)) poss

label :: Direction -> Puzzle :-> [Clue Regex]
label Horizontal = horizontal
label Vertical   = vertical
label Diagonal   = diagonal

row :: Direction -> Position -> Length -> [Position]
row dir pos len = take len . iterate (next dir) $ pos
  where
    next Horizontal = first  (+1)
    next Vertical   = second (+1)
    next Diagonal   = subtract 1 *** subtract 1

updateClue :: Clue a -> [Clue a] -> [Clue a]
updateClue c = map (\c' -> if D.position c == D.position c' then c else c')

showBoard :: Board -> String
showBoard = intercalate "\n"
          . map showRow
          . groupBy (equating (snd . fst))
          . sortBy (comparing (swap . fst))
          . Map.toList

showRow :: [(Position, Atom)] -> String
showRow = go 0
  where
    go _ [] = ""
    go n l@(((x, _), a):ps) | n < x     = ' ' : go (n+1) l
                            | n == x    = pprAtom a ++ go (n+1) ps
                            | otherwise = "" -- Shouldn't happen.

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating = on (==)

foldMapEndo :: (Foldable f) => (a -> b -> b) -> f a -> b -> b
foldMapEndo f = appEndo . foldMap (Endo . f)
