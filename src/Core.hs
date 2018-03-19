module Core
  ( eval
  , evalWithM
  ) where

import           Syntax
import           Exp

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
