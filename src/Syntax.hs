module Syntax
  ( pprint
  , fmtExp
  , expSubstTop
  , Binding(..)
  -- , CContext
  ) where

import           Exp

data Binding =
  NameBind
  deriving (Show) -- this will be extended in later chapters

type CContext = [(String, Binding)]

-- XXX: remove me
cctxtfromctx = map(\x-> (x,NameBind))

{-
 - shifting
 -}
expShift :: Int -> Exp -> Exp
expShift d = expShiftAbove d 0

expShiftAbove :: Int -> Int -> Exp -> Exp
expShiftAbove d =
  expMap
    (\iden c x n ->
       if x >= c
         then Var iden (x + d) (n + d)
         else Var iden x (n + d))

expMap :: (String -> Int -> Int -> Int -> Exp) -> Int -> Exp -> Exp
expMap onvar c (Var iden x n) = onvar iden c x n
expMap onvar c (Abs x t) = Abs x (expMap onvar (c + 1) t)
expMap onvar c (App t1 t2) = App (expMap onvar c t1) (expMap onvar c t2)

{-
 - substitution
 -}
expSubst :: Int -> Exp -> Exp -> Exp
expSubst j s =
  expMap
    (\iden c x n ->
       if x == j + c
         then expShift c s
         else Var iden x n)
    0

expSubstTop ::  Exp -> Exp -> Exp
expSubstTop s t = expShift (-1) (expSubst 0 (expShift 1 s) t)

{-
 - simple pretty printer
 -}
pprint :: [String] -> Exp -> IO ()
pprint ctx = putStrLn . fmtExp (cctxtfromctx ctx)

fmtExp :: CContext -> Exp -> String
fmtExp ctx (Var _ x n) =
  if ctxLength ctx >= n
    then indexToName ctx x
    else "bad index"
fmtExp ctx (Abs x t) =
  let (ctx', x') = pickFreshName ctx x
  in "(\\" ++ x' ++ "." ++ fmtExp ctx' t ++ ")"
  -- in "(λ" ++ x' ++ "." ++ fmtExp ctx' t ++ ")"
  -- in "(lambda " ++ x' ++ "." ++ fmtExp ctx' t ++ ")"
fmtExp ctx (App t1 t2) =
  "(" ++ fmtExp ctx t1 ++ " " ++ fmtExp ctx t2 ++ ")"

isNameBound :: CContext -> String -> Bool
isNameBound ctx x = any ((x ==) . fst) ctx

pickFreshName :: CContext -> String -> (CContext, String)
pickFreshName ctx x =
  if isNameBound ctx x
    then pickFreshName ctx (x ++ "'")
    else ((x, NameBind) : ctx, x)

indexToName :: CContext -> Int -> String
indexToName ctx x = fst $ (ctx !! x)

ctxLength :: CContext -> Int
ctxLength = length
