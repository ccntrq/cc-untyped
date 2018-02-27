module Main where

import           Core
import           Parser
import           Syntax
import           Term

import           Control.Monad (when)
import           System.IO

version :: String
version = "cc-untyped v0.1.0"

main :: IO ()
main = repl

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStrLn version
  putStrLn "Welcome to the cc-untyped repl!"
  putStrLn "Please enter a lambda term or type 'quit' to exit"
  repl'
  where
    repl' = do
      putStr "λ:"
      input <- getLine
      when (input /= "quit") (smallStep input >> repl')

pprint :: String -> IO ()
pprint = either print printTerm . parse

eval' = reduceWithStrategy eval "call by value"

reduceWithStrategy :: (Term -> Term) -> String -> String -> IO ()
reduceWithStrategy strategy strategyName src =
  case parse src of
    Left err -> print err >> putStrLn src
    Right tm -> do
      putStrLn $ "Reducing with '" ++ strategyName ++ "' strategy :"
      printTerm tm
      putStrLn "Result:"
      printTerm $ strategy tm

-- smallstep call by value reduction
smallStep :: String -> IO ()
smallStep src =
  case parse src of
    Left err -> putStrLn src >> print err
    Right tm -> do
      evalWithM (putStrLn . ("Reducing: \n" ++) . fmtTerm []) tm
      putStrLn "Done."

{-
 - predefined functions
 -}
lcId = "(\\x.x)"

{-
 - pairs
 -}
pair = "(\\f.\\s.\\b.b f s)"

lcFst = "(\\p.p " ++ tru ++ ")"

lcSnd = "(\\p.p " ++ fls ++ ")"

{-
 - Church Booleans
 -}
tru = "(\\t.\\f.t)"

fls = "(\\t.\\f.f)"

test = "(\\l.\\m.\\n.l m n)"

{-
 - Church Numbers
 -}
plus = "(\\m.\\n.\\s.\\z.m s (n s z))"

times = "(\\m.\\n.m(" ++ plus ++ " n)" ++ c0 ++ ")"

scc = "(\\n.\\s.\\z.s (n s z))"

zz = pair ++ c0 ++ c0

ss =
  "(\\p." ++
  pair ++ "(" ++ lcSnd ++ "p)" ++ "(" ++ plus ++ c1 ++ "(" ++ lcSnd ++ "p)))"

prd = "(\\m." ++ lcFst ++ "(m" ++ ss ++ zz ++ "))"

iszero = "(\\n.n(\\x." ++ fls ++ ")" ++ tru ++ ")"

c0 = "(\\s.\\z.z)"

c1 = "(\\s.\\z.s z)"

c2 = "(\\s.\\z.s (s z))"

c3 = "(\\s.\\z.s (s (s z)))"
