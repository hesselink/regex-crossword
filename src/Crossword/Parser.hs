{-# LANGUAGE NoMonomorphismRestriction #-}
module Crossword.Parser where

import Prelude hiding (any, seq)

import Control.Applicative
import Data.Maybe
import Text.Parsec hiding (oneOf, noneOf, (<|>), choice)
-- import Data.Char
-- import Text.ParserCombinators.UU hiding (Seq)
-- import Text.ParserCombinators.UU.BasicInstances

import Crossword.Regex

-- char = pSym
-- upper = pSatisfy isUpper (Insertion "" '#' maxBound)
-- digit = pSatisfy isDigit (Insertion "" '#' maxBound)
-- string = pToken
-- sepBy1 = flip pList1Sep

regex = choices <$> sepBy1 regex' (char '|')

regex' = seq <$> some (withModifier $
   (  literal
  <|> any
  <|> oneOrNone
  <|> backRef
  <|> group
   )
  )

withModifier p = p <**> (option id modifier)

modifier =  Many   <$ char '*'
        <|> Many1  <$ char '+'
        <|> Option <$ char '?'

literal = Literal . Token <$> upper

any = Any <$ char '.'

oneOf = OneOf . map Token <$> some upper <* char ']'

noneOf = NoneOf . map Token <$ char '^' <*> some upper <* char ']'

oneOrNone = char '[' *> (oneOf <|> noneOf)

many_ = Many <$> regex <* char '*'

many1_ = Many1 <$> regex <* char '+'

group = Group <$ char '(' <*> regex <* char ')'

backRef = BackRef . read . pure <$ char '\\' <*> digit

choice = choices <$ char '(' <*> sepBy1 regex (char '|') <* char ')'

-- option = Option <$> regex <* char '?'

seq = foldr1 Seq

choices = foldr1 Choice