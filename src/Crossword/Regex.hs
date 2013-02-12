{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crossword.Regex where

newtype Token = Token { unToken :: Char } deriving (Show, Eq, Enum)

instance Bounded Token where
  minBound = Token 'A'
  maxBound = Token 'Z'

enumAll :: (Bounded a, Enum a) => [a]
enumAll = [minBound .. maxBound]

data Regex =
    Literal Token
  | Any
  | OneOf [Token]
  | NoneOf [Token]
  | Many Regex
  | Many1 Regex
  | Seq Regex Regex
  | Group Regex
  | BackRef Int
  | Choice Regex Regex
  | Option Regex
  deriving Show

lit :: Char -> Regex
lit = Literal . Token
