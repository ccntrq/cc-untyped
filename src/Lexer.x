{
module Lexer (evalAlex, getState, getUserState, runAlex, Alex, AlexUserState(..), TokenType(..), lexer, lexIt, Token(..), AlexState(..),addLog)  where
import Debug.Trace
}

%wrapper "monadUserState"

$whitespace = [\ \t\b\n]
$alpha = [a-zA-Z]       -- alphabetic characters
state:-

<0>       $whitespace+        ;
<0>       "#".*	   	       ;
<0>       $alpha [$alpha \']* { addToken (Iden "")}  -- add dummy string
<0>       [\\]                { addToken Lam}
<0>       [\.]                { addToken Dot}
<0>       [\(]                { addToken LPar }
<0>       [\)]                { addToken RPar  }

{

-- a Token stores its TokenType, position and the lexed characters
data Token = Token TokenType AlexPosn (Maybe String) deriving(Eq,Show)

data TokenType
  = Iden String
  | Lam
  | Dot
  | LPar
  | RPar
  | EOF
  deriving (Eq,Show)

-- setup monadUserState infrastructure

-- XXX dummy
data AlexUserState = AlexUserState
  { inputLine :: Int -- unused
  , parserLog :: [String]
  , boundContext :: [String]
  } deriving(Show,Eq)

addLog :: String -> Alex ()
addLog l = Alex $ \s -> Right (s{alex_ust=let us = alex_ust s in us{parserLog=l:(parserLog us)}},())

pushContext :: String -> Alex ()
pushContext var = Alex $ \s -> Right (s{alex_ust=let us = alex_ust s in us{boundContext=var:(boundContext us)}},())

popContext :: Alex ()
popContext = Alex $ \s -> Right (s{alex_ust=let us = alex_ust s in us{boundContext=(drop 1 $ boundContext us)}},())

getUserState :: Alex AlexUserState
getUserState = Alex $ \s@AlexState{alex_ust=us} -> Right (s, us)

getState :: Alex AlexState
getState = Alex $ \s -> Right (s, s)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 1 [] []


alexEOF :: Alex Token
-- TODO: last token pos instead of undefined?
alexEOF = return $ Token EOF undefined Nothing

addToken :: TokenType -> AlexInput -> Int -> Alex Token
-- for 'Iden's we have to replace the dummy string with the real lexed name here
addToken (Iden _) (p, _, _, str) len = return $ Token (Iden lexed) p (Just lexed)
  where lexed = take len str
addToken t (p, _, _, str) len = return $ Token t p (Just $ take len str)

-- Interface

-- lex the given input string and return a error message or a result
lexIt :: String -> Either String [Token]
lexIt input = runAlex input loop
  where
    loop = do
      token <- alexMonadScan
      case token of
        (Token EOF _ _) -> return []
        _ -> loop >>= return . (token:)

-- this is where the happy parser will hook into
lexer :: (Token -> Alex a) -> Alex a
lexer cont = do
  token <- alexMonadScan
  case token of
    (Token EOF _ _) -> cont token
    _ -> do
      addLog $ "lexed:" ++ (show token)
      cont token


evalAlex :: String -> Alex a -> Either String (AlexUserState, a )
evalAlex input (Alex f) =
     case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,
                        alex_chr = '\n',
                        alex_bytes = [],
                        alex_ust = alexInitUserState,
                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( s, a ) -> Right (alex_ust s, a)
}
