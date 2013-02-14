{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crossword.Token where

newtype Token = Token { unToken :: Char } deriving (Show, Eq, Ord, Enum)

instance Bounded Token where
  minBound = Token 'A'
  maxBound = Token 'Z'

enumAll :: (Bounded a, Enum a) => [a]
enumAll = [minBound .. maxBound]

