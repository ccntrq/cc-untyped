module Main where

import           Core
import           Parser
import           Syntax

main :: IO ()
main =
  let terms = ["((lambda a.a)(lambda a.a))", "\\a.\\b.\\c.\\d.a b c d"]
  in mapM_ (\x -> smallStep x >> bigStep x) terms

pprint :: String -> IO ()
pprint = either print printTerm . parse

bigStep :: String -> IO ()
bigStep src =
  case parse src of
    Left err -> print err
    Right tm -> do
      putStrLn "Reducing:"
      printTerm tm
      putStrLn "Result:"
      printTerm $ eval [] tm

smallStep :: String -> IO ()
smallStep src =
  case parse src of
    Left err -> print err
    Right tm -> do
      evalWithM (putStrLn . ("Reducing: \n" ++) . fmtTerm []) [] tm
      putStrLn "Done."
