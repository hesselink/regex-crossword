{-# LANGUAGE DeriveFunctor #-}
module Crossword.Description where

import Text.Parsec hiding ((<|>))

import Crossword.Parser (Parser)
import Crossword.Regex

import qualified Crossword.Parser as P

data Description = Description
  { horizontal :: [Clue Regex]
  , vertical   :: [Clue Regex]
  , diagonal   :: [Clue Regex]
  } deriving Show

description :: Parser Description
description = Description <$> (string "Horizontal" *> newline *> many1 (pClue <* newline))
                          <*  newline
                          <*> (string "Vertical"   *> newline *> many1 (pClue <* newline))
                          <*  newline
                          <*> (string "Diagonal"   *> newline *> many1 (pClue <* newline))

type Position = (Int, Int)
type Length = Int

data Clue a = Clue
  { position :: Position
  , len      :: Length
  , clue     :: a
  } deriving (Show, Functor)

pClue :: Parser (Clue Regex)
pClue = Clue <$> pPosition <* spaces <*> number <* spaces <*> P.regex

pPosition :: Parser (Int, Int)
pPosition = (,) <$ char '(' <*> number <* char ',' <*> number <* char ')'

number :: Parser Int
number = read <$> many1 digit
