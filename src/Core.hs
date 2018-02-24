module Core
  ( eval1
  , eval
  , evalWithM
  ) where

import           Syntax
import           Term

data NoRuleApplies =
  NoRuleApplies

isVal :: Term -> Bool
isVal TmAbs {} = True
isVal _        = False

eval1 :: Context -> Term -> Either NoRuleApplies Term
eval1 ctx (TmApp _ (TmAbs _ x t) v)
  | isVal v = return $ termSubstTop v t
eval1 ctx (TmApp fi v t)
  | isVal v = do
    t' <- eval1 ctx t
    return $ TmApp fi v t'
eval1 ctx (TmApp fi t1 t2) = do
  t1' <- eval1 ctx t1
  return $ TmApp fi t1' t2
eval1 _ _ = Left NoRuleApplies

eval :: Context -> Term -> Term
eval ctx t = either (const t) (eval ctx) (eval1 ctx t)

evalWithM :: Monad m => (Term -> m a) -> Context -> Term -> m Term
evalWithM fn ctx t = do
  fn t
  case eval1 ctx t of
    Left NoRuleApplies -> return t
    Right t'           -> evalWithM fn ctx t'
