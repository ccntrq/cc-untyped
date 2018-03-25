module Main where

import           Core
import           Exp
import           Lexer
import           Parser
import           Syntax

import           Control.Monad      (unless)

import           System.Directory   (doesFileExist)
import           System.Environment (getArgs, getProgName)
import           System.IO

version :: String
version = "cc-untyped v0.3.0"

usage :: String -> String
usage progname =
  "Usage: " ++
  progname ++
  " [-h] files?\n" ++
  "# " ++
  version ++
  "\n" ++
  "You can pass a list of files to execute them sequentially or invoke the\n" ++
  "interpreter without any args to enter a repl"

freeVariables :: Context
freeVariables = ["a", "b", "c", "d"] -- allowed free variables

defaultReducer :: Reducer
defaultReducer= normalOrder

main :: IO ()
main = do
  args <- getArgs
  dispatch args

-- dispatch to usage, repl or runfiles
dispatch :: [String] -> IO ()
dispatch ["-h"] = getProgName >>= putStrLn . usage
dispatch []     = repl defaultReducer
dispatch files  = runFiles defaultReducer files



runFiles :: Reducer -> [String] -> IO ()
runFiles reducer =
  mapM_ (\f -> putStrLn ("Starting execution of file: " ++ f) >> runFile reducer f)

runFile :: Reducer -> String -> IO ()
runFile reducer file = do
  doesExist <- doesFileExist file
  unless doesExist (error $ "File does not exist: " ++ file)
  src <- readFile file
  runSource reducer src

repl :: Reducer -> IO ()
repl reducer = do
  hSetBuffering stdout NoBuffering
  putStrLn version
  putStrLn "Welcome to the cc-untyped repl!"
  putStrLn "Please enter a lambda term or type 'quit' to exit"
  repl'
  where
    repl' = do
      putStr "λ:"
      input <- getLine
      unless (input == "quit") (runSource reducer input >> repl')

runSource :: Reducer -> String -> IO ()
runSource reducer src =
  case evalAlex src (parseIt freeVariables) of
    Left e -> print e
    Right (_, e)
      -- evalWithM (\e' -> (putStrLn $ "reducing: " ++ show e') >> pprint freeVariables e') e
     -> do
      putStr "parsed : "
      pprint freeVariables e
      putStr "reduced: "
      let res = (reducer e)
      pprint freeVariables res
