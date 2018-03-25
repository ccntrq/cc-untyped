{
module Parser(parseIt) where
import Lexer
import Exp
import Data.List
import Data.Maybe
}

%name parse
%tokentype { Token }

%monad { Alex }
%lexer { lexer } { Token EOF _ _ }
%error { parserError }

%token
			iden    { Token (Iden $$) _ _ }
			'\\'    { Token Lam _ _       }
			'.'     { Token Dot _ _       }
			'('     { Token LPar _ _      }
			')'     { Token RPar _ _      }
			let     { Token Let _ _       }
			in      { Token In _ _        }
			'='     { Token Equ _ _       }


%%

Exp : '\\' iden '.' Exp        {% mkAbsExp $2 $4 }
    | let iden '=' Exp in Exp  {% desugarLet $2 $4 $6 }
    | AppExp                   {% return $1      }

AppExp : AppExp AtomExp    {% mkAppExp $1 $2 }
       | AtomExp           {% return $1      }

AtomExp : iden             {% mkVarExp $1    }
        | '(' Exp ')'      {% return $2      }

{

{-
 - We are using de Brujin indices in our internal represantation for lambda
 - expressions. To allow our users to use named terms we have calculate the
 - incices in the parser. With happy we have a LR parser and are thus building
 - the expressions from bottom up. This means that the actions for Var
 - expressions are executed before the action for their binders and they cannot
 - know their context yet. To still be able to build the expressions in one go
 - the parser actions return a function that expects the bound context for that
 - expression and builds the expression using that context.  You can then pass
 - the top-level bound expressions to the function returned by the parser to
 - build the ast.
 -}

parseIt :: Context -> Alex Exp
parseIt ctx = parse `ap` return ctx

mkAbsExp :: String -> (Context -> Exp) -> Alex (Context -> Exp)
mkAbsExp param body = return $ \ctx -> let ctx' = param:ctx in Abs param (body ctx')

mkAppExp :: (Context -> Exp) -> (Context -> Exp) -> Alex (Context -> Exp)
mkAppExp e1 e2 = return $ \ctx -> App (e1 ctx) (e2 ctx)

mkVarExp :: String -> Alex (Context -> Exp)
mkVarExp name =
  return $ \ctx -> case elemIndex name ctx of
                     Nothing -> error $ "Variable not bound: " ++ name -- XXX
                     Just index -> Var name index (length ctx)

-- we desugar let bindings into the core calculus
-- from: let name = e1 in e2
-- to:   (\name. e2) e1
desugarLet :: String -> (Context -> Exp) -> (Context -> Exp) -> Alex (Context -> Exp)
desugarLet name e1 e2 = return $ \ctx -> App (Abs name (e2 (name:ctx)))(e1 ctx)

parserError :: Token -> Alex a
parserError (Token t (AlexPn _ l c) lexeme) =
  fail $ "parsing error at line " ++ (show l) ++ ", column "  ++ (show c) ++
         ": '" ++ lexeme ++ "' " ++ (show t)
}
