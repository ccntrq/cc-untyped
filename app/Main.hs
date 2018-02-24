module Main where

import           Parser
import           Syntax

main :: IO ()
main = do
  pprint "\\a.a"
  pprint "((\\a.lambda a.a) (\\a.a))"
  pprint "\\x.\\y.x y"
  pprint "\\f.(\\x.f (x x)) (\\x.f (x x))" -- Y combinator
  pprint "((lambda a.a)(lambda a.a))"

pprint :: String -> IO ()
pprint = either print printTerm . parse
