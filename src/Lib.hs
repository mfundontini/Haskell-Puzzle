module Lib
    ( someFunc,
      myFunction,
      languages,
      grid,
      formatGrid,
      anotherFormatter,
      outputGrid,
      isWordInGrid
    ) where

import Data.List

type Grid = [String]

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myFunction :: String
myFunction = "Hello World"

grid = [ "__C________R___"
       , "__SI________U__"
       , "__HASKELL____B_"
       , "__A__A_____S__Y"
       , "__R___B___C____"
       , "__PHP____H_____"
       , "____S_LREP_____"
       , "____I__M_Y__L__"
       , "____L_E__T_O___"
       , "_________HB____"
       , "_________O_____"
       , "________CN_____"
       ]

languages = [ "BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"
            ]

formatGrid :: Grid -> String
formatGrid grid = unwords $ map (\s -> s ++ "\n") grid

anotherFormatter :: String -> Grid -> String
anotherFormatter formater grid = intercalate formater grid

outputGrid :: IO ()
outputGrid = putStrLn $ unlines grid

isWordInGrid :: String -> Grid -> Bool
isWordInGrid _ [] = False
isWordInGrid word (begin:remainder) = if  word `isInfixOf` begin then True else isWordInGrid word remainder
