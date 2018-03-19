module Main where

import           Lexer
import           Parser
import           Core

import           Control.Monad (when)
import           System.IO

version :: String
version = "cc-untyped v0.2.0"

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
      when (input /= "quit") (runSource input >> repl')

runSource :: String -> IO ()
runSource src =
  case evalAlex src parseIt of
    Left e -> print e
    Right (_, e) -> do
      evalWithM (\e' -> putStrLn $ "reducing: " ++ show e') e
      putStrLn "Done."
