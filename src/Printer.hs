module Printer where

import           Term

data Binding =
  NameBind
  deriving (Show) -- this will be extended in later chapters

type Context = [(String, Binding)]

printTerm :: Term -> IO ()
printTerm = putStrLn . fmtTerm []

fmtTerm :: Context -> Term -> String
fmtTerm ctx (TmVar fi x n) =
  if ctxLength ctx == n
    then indexToName fi ctx x
    else "bad index"
fmtTerm ctx (TmAbs fi x t) =
  let (ctx', x') = pickFreshName ctx x
  in "(lambda " ++ x' ++ "." ++ fmtTerm ctx' t ++ ")"
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
