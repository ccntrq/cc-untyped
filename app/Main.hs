module Main where

import           Core
import           Parser
import           Syntax

main :: IO ()
main =
  let terms =
        [ "((lambda a.a)(lambda a.a))"
        , "\\a.\\b.\\c.\\d.a b c d"
        , tru
        , fls
        , test
        , c0
        , c1
        , c2
        , c3
        , test ++ tru ++ c3 ++ c0
        , test ++ fls ++ c0 ++ c3
        ]
  in mapM_ smallStep terms

pprint :: String -> IO ()
pprint = either print printTerm . parse

-- TODO: add possibility to choose
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
    Left err -> putStrLn src >> print err
    Right tm -> do
      evalWithM (putStrLn . ("Reducing: \n" ++) . fmtTerm []) [] tm
      putStrLn "Done."

{-
 - Church Booleans
 -}
tru = "(\\t.\\f.t)"

fls = "(\\t.\\f.f)"

test = "(\\l.\\m.\\n.l m n)"

{-
 - Church Numbers
 -}
c0 = "\\s.\\z.z"

c1 = "\\s.\\z.s z"

c2 = "\\s.\\z.s (s z)"

c3 = "\\s.\\z.s (s (s z))"
