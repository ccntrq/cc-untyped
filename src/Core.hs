module Core
  ( eval
  , evalWithM
  , callByName
  , normalOrder
  , Reducer
  ) where

import           Exp
import           Syntax

data NoRuleApplies =
  NoRuleApplies

isVal :: Exp -> Bool
isVal Abs {} = True
isVal Var {} = True -- we want to allow free variables.
isVal _      = False

-- call by value reduction
-- evaluate the rightmost outermost redex, but not under abstractions, and only
-- if the right hand side of the redex is a value
eval1 :: Exp -> Either NoRuleApplies Exp
eval1 (App (Abs x t) v)
  | isVal v = return $ expSubstTop v t
eval1 (App v t)
  | isVal v = do
    t' <- eval1 t
    return $ App v t'
eval1 (App t1 t2) = do
  t1' <- eval1 t1
  return $ App t1' t2
eval1 _ = Left NoRuleApplies

eval :: Exp -> Exp
eval t = either (const t) eval (eval1 t)

evalWithM :: Monad m => (Exp -> m a) -> Exp -> m ()
evalWithM fn t = do
  fn t
  case eval1 t of
    Left NoRuleApplies -> return ()
    Right t'           -> evalWithM fn t'

{-
 - The following reducers are implemented based on algorithms from the paper
 - 'Demonstrating Lambda Calculus Reduction' by Peter Sestoft. You can find the
 - paper at https://www.itu.dk/people/sestoft/papers/sestoft-lamreduce.pdf
 -}

type Reducer = Exp -> Exp

callByName :: Reducer
callByName x@Var {} = x
callByName (Abs x e) = Abs x e
callByName (App e1 e2) =
  case callByName e1 of
    Abs x e -> callByName (expSubstTop e2 e)
    e1'     -> App e1' e2

normalOrder :: Reducer
normalOrder x@Var {} = x
normalOrder (Abs x e) = Abs x (normalOrder e)
normalOrder (App e1 e2) =
  case callByName e1 of
    Abs x e -> normalOrder (expSubstTop e2 e)
    e1' ->
      let e1'' = normalOrder e1'
      in App e1'' (normalOrder e2)
