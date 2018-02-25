module Syntax
  ( printTerm
  , fmtTerm
  , termSubstTop
  , Binding(..)
  , Context
  ) where

import           Term

data Binding =
  NameBind
  deriving (Show) -- this will be extended in later chapters

type Context = [(String, Binding)]

{-
 - shifting
 -}
termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
  tmMap
    (\fi c x n ->
       if x >= c
         then TmVar fi (x + d) (n + d)
         else TmVar fi x (n + d))

tmMap :: (Info -> Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmMap onvar c (TmVar fi x n) = onvar fi c x n
tmMap onvar c (TmAbs fi x t) = TmAbs fi x (tmMap onvar (c + 1) t)
tmMap onvar c (TmApp fi t1 t2) = TmApp fi (tmMap onvar c t1) (tmMap onvar c t2)

{-
 - substitution
 -}
termSubst :: Int -> Term -> Term -> Term
termSubst j s =
  tmMap
    (\fi c x n ->
       if x == j + c
         then termShift c s
         else TmVar fi x n)
    0

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

{-
 - simple pretty printer
 -}
printTerm :: Term -> IO ()
printTerm = putStrLn . fmtTerm []

fmtTerm :: Context -> Term -> String
fmtTerm ctx (TmVar fi x n) =
  if ctxLength ctx >= n
    then indexToName fi ctx x
    else "bad index"
fmtTerm ctx (TmAbs fi x t) =
  let (ctx', x') = pickFreshName ctx x
  in "(λ" ++ x' ++ "." ++ fmtTerm ctx' t ++ ")"
  -- in "(lambda " ++ x' ++ "." ++ fmtTerm ctx' t ++ ")"
fmtTerm ctx (TmApp fi t1 t2) =
  "(" ++ fmtTerm ctx t1 ++ " " ++ fmtTerm ctx t2 ++ ")"

isNameBound :: Context -> String -> Bool
isNameBound ctx x = any ((x ==) . fst) ctx

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x =
  if isNameBound ctx x
    then pickFreshName ctx (x ++ "'")
    else ((x, NameBind) : ctx, x)

indexToName :: Info -> Context -> Int -> String
indexToName fi ctx x = fst $ (ctx !! x)

ctxLength :: Context -> Int
ctxLength = length
