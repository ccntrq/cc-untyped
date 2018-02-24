module Parser
  ( parse
  ) where

{-
   Parser for the untyped lambda calculus.
   The parser is ipmlemented using the Parsec parser library. As a first time
   user I was happy to find a tutorial using the same book I am reading. Most
   of the parser is taken from there:
   http://mattwetmore.me/posts/parsing-combinators-with-parser-combinators.html
 -}
import           Term

import           Data.List              (elemIndex)
import           Text.Parsec            hiding (parse)
import           Text.Parsec.Combinator (between, chainr1, sepBy1)

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

parseTerm :: LCParser
parseTerm =
  chainl1 parseNonApp $ do
    spaces
    pos <- getPosition
    return $ TmApp (infoFrom pos)

parseNonApp :: LCParser
parseNonApp =
  parens parseTerm -- (M)
   <|>
  parseAbs -- $\lambda$x.M
   <|>
  parseVar -- x

parseAbs :: LCParser
parseAbs = do
  char '\\' <|> (lambda >> space)
  v <- parseVarName
  modifyState (v :)
  char '.'
  term <- parseTerm
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

-- | parser helper for variable names.
parseVarName :: Parsec String u String
parseVarName = many1 $ letter <|> char '\''

-- | parser helper for parens.
parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

lambda = char 'l' >> char 'a' >> char 'm' >> char 'b' >> char 'd' >> char 'a'

-- | A helper to create our info annotations from parsecs SourcePos type
infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)
