module Main where

import           Parser

main :: IO ()
main = do
  print $ parse "\\aa.aa"
