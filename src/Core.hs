module Core
  ( eval
  , evalWithM
  ) where

import           Syntax
import           Term

data NoRuleApplies =
  NoRuleApplies

isVal :: Term -> Bool
isVal TmAbs {} = True
isVal _        = False

eval1 :: Term -> Either NoRuleApplies Term
eval1 (TmApp _ (TmAbs _ x t) v)
  | isVal v = return $ termSubstTop v t
eval1 (TmApp fi v t)
  | isVal v = do
    t' <- eval1 t
    return $ TmApp fi v t'
eval1 (TmApp fi t1 t2) = do
  t1' <- eval1 t1
  return $ TmApp fi t1' t2
eval1 _ = Left NoRuleApplies

eval :: Term -> Term
eval t = either (const t) eval (eval1 t)

evalWithM :: Monad m => (Term -> m a) -> Term -> m Term
evalWithM fn t = do
  fn t
  case eval1 t of
    Left NoRuleApplies -> return t
    Right t'           -> evalWithM fn t'
