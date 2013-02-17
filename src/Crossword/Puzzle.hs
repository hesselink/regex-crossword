{-# LANGUAGE TupleSections, TemplateHaskell, TypeOperators #-}
module Crossword.Puzzle where

import Control.Arrow
import Control.Newtype
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

import Crossword.Expand
import Crossword.FixedRegex
import Crossword.Description (Description, Position, Length, Clue)

import qualified Crossword.Description as D

type Board = Map Position Atom

data Puzzle = Puzzle
  { _board      :: Board
  , _horizontal :: [Clue [FixedRegex]]
  , _vertical   :: [Clue [FixedRegex]]
  , _diagonal   :: [Clue [FixedRegex]]
  } deriving Show

mkLabel ''Puzzle

initialize :: Description -> Puzzle
initialize descr =
  let setRowToAny :: [Position] -> Board -> Board
      setRowToAny = foldMapEndo (flip Map.insert Any)
      horizontalRows = map (uncurry (row Horizontal) . (D.position &&& D.len)) . D.horizontal
      initialBoard = (foldMapEndo setRowToAny . horizontalRows $ descr) Map.empty
      expandClue c = c { D.clue = expand (D.len c) (D.clue c) }
      expanded direction = map expandClue . direction $ descr
  in Puzzle initialBoard (expanded D.horizontal) (expanded D.vertical) (expanded D.diagonal)

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

stepClue :: Direction -> Clue [FixedRegex] -> Puzzle -> Puzzle
stepClue dir c pzl = modify (label dir) (updateClue (c { D.clue = newOpts }))
                   . modify board updateBoard
                   $ pzl
  where
    updateBoard = foldMapEndo (uncurry (Map.insertWith (\x -> fromJust . intersectAtom x))) $ zip poss newRow
    newRow = merges newOpts
    isPossible = isJust . intersect curRow
    newOpts = filter isPossible (D.clue c)
    poss = row dir (D.position c) (D.len c)
    curRow = map (fromJust . flip Map.lookup (get board pzl)) poss

label :: Direction -> Puzzle :-> [Clue [FixedRegex]]
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

foldMapEndo :: (Functor f, Foldable f) => (a -> b -> b) -> f a -> b -> b
foldMapEndo f = ala Endo foldMap . fmap f
