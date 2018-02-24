module Term
  ( Term(..)
  , Info(..)
  ) where

{-|
   Terms of the untyped lambda calculus. Each term carries an Info annotation
   with row and col of the token with it.
 -}
data Term
  -- |
  -- Variables carry their de Bruijn indices and the total length of the
  -- context they appear in. This number is used as consistency check
  = TmVar Info
          Int
          Int
  -- |
  -- Abstraction carry a name hint that is used in the pretty printer. A few
  -- primes might get added to avoid name clashes
  | TmAbs Info
          String
          Term
  -- | Applications are just two terms callee and an argument
  | TmApp Info
          Term
          Term
  deriving (Show)

{-|
  The Info type ise used to carry positional information for debugging purposes
 -}
data Info = Info
  { row :: Int
  , col :: Int
  } deriving (Show)
