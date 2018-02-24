module Parser
  ( parse
  ) where

import           Data.List              (elemIndex)
import           Text.Parsec            hiding (parse)
import           Text.Parsec.Combinator (between, chainr1, sepBy1)

{-
   Parser for the untyped lambda calculus.
   The parser is ipmlemented using the Parsec parser library. As a first time
   user I was happy to find a tutorial using the same book I am reading. Most
   of the parser is taken from there:
   http://mattwetmore.me/posts/parsing-combinators-with-parser-combinators.html
 -}
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

-- |
-- An alias used to store the bound variables in a context.
-- It is used to calculate the de Bruijn indices
type BoundContext = [String]

-- | a parser type for the untyped lambda calculus parser
type LCParser = Parsec String BoundContext Term

-- | helper to run our parsers
parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "untyped lambda-calculus"

-- | The main parse function for lambda terms
parse :: String -> Either ParseError Term
parse = parseWith parseTerm

parseAbs :: LCParser -> LCParser
parseAbs termParser = do
  char '\\'
  v <- parseVarName
  modifyState (v :)
  char '.'
  term <- termParser
  modifyState tail
  pos <- getPosition
  return $ TmAbs (infoFrom pos) v term

parseVar :: LCParser
parseVar = do
  v <- parseVarName
  list <- getState
  findVar v list

findVar :: String -> BoundContext -> LCParser
findVar v list =
  case elemIndex v list of
    Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
    Just n -> do
      pos <- getPosition
      return $ TmVar (infoFrom pos) n (length list)

parseNonApp :: LCParser
parseNonApp =
  parens parseTerm -- (M)
   <|>
  parseAbs parseTerm -- $\lambda$x.M
   <|>
  parseVar -- x

parseTerm :: LCParser
parseTerm =
  chainl1 parseNonApp $ do
    space
    pos <- getPosition
    return $ TmApp (infoFrom pos)

-- | parser helper for variable names.
parseVarName :: Parsec String u String
parseVarName = many1 $ letter <|> char '\''

-- | parser helper for parens.
parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

-- | A helper to create our info annotations from parsecs SourcePos type
infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)
