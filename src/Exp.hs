module Exp
  ( Exp(..)
  , Context
  ) where

-- XXX Syntax
type Context  = [String] -- just a list of the bound variables for now

{-
 - Expressions.
 - In a Var we store its original name from the input, its de brujin index
 - and the number of visible binders in the scope of this variable
 -}
data Exp
  = Var String
        Int
        Int
  | Abs String
        Exp
  | App Exp
        Exp
  deriving (Show)
