-- Exports
module Lib
    ( someFunc,
      myFunction,
      formatGrid,
      anotherFormatter,
      outputGrid,
      outputSearchWords,
      isWordInGrid,
      isWordInReverse,
      areMultipleWordsInGrid,
      checkForWordsComplete,
      checkForWordsGeneric,
      transformGrid,
      undiagonalized
    ) where

-- Imports
import Data.List
import Data.Maybe (catMaybes)
import Const (languages, grid, Grid, play)

-- Test function for compilation
someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- test function for unit testing
myFunction :: String
myFunction = "Hello World"

-- Format the grid using map
formatGrid :: Grid -> String
formatGrid grid = unwords $ map (\s -> s ++ "\n") grid

-- Format the grid using intercalate
anotherFormatter :: String -> Grid -> String
anotherFormatter formater grid = intercalate formater grid

-- Format and output grid using unlines
outputGrid :: IO ()
outputGrid = putStrLn $ unlines grid

-- Format and output grid using search words
outputSearchWords :: IO ()
outputSearchWords = putStrLn $ unlines languages

-- Check if the word is in a normalized grid, then reverse the word and check again
isWordInGrid :: String -> Grid -> Maybe String
isWordInGrid _ [] = Nothing
isWordInGrid word (begin:remainder) = if  word `isInfixOf` begin || ( (reverse word) `isInfixOf` begin) then Just word else isWordInGrid word remainder

-- Check the reverse of the word is in the Grid instead of the grid, this is not used
isWordInReverse :: String -> Grid -> Maybe String
isWordInReverse word grid = isWordInGrid (reverse word) grid

-- Call isWord in grid multiple times using a list of words
areMultipleWordsInGrid :: Grid -> [String] -> [String]
areMultipleWordsInGrid [] [] = []
areMultipleWordsInGrid grid list = catMaybes $ map (\s -> isWordInGrid s grid) list

-- Complete function for checking words
-- TO:DO clean up this inefficient logic
checkForWordsComplete :: [String]
checkForWordsComplete =
    (areMultipleWordsInGrid grid languages) ++
    (areMultipleWordsInGrid (transpose grid) languages) ++
    (areMultipleWordsInGrid (transpose (undiagonalized grid 45)) languages) ++
    (areMultipleWordsInGrid (transpose (undiagonalized grid 135)) languages)


-- Generic word searcher
checkForWordsGeneric :: Grid -> [String] -> [String]
checkForWordsGeneric [] [] = []
checkForWordsGeneric someGrid searchWords =
    (areMultipleWordsInGrid someGrid searchWords) ++
    (areMultipleWordsInGrid (transpose someGrid) searchWords) ++
    (areMultipleWordsInGrid (transpose (undiagonalized someGrid 45)) searchWords) ++
    (areMultipleWordsInGrid (transpose (undiagonalized someGrid 135)) searchWords)


-- Coverts 45 and 135 degree diagonals to vertical
transformGrid :: Grid -> Int -> [Maybe String]
transformGrid [] _ = [Nothing]
transformGrid (begin:remainder) degrees = let
    offset = (length begin) - 1
    prefix = offset - ((length remainder) + 1)
    suffix = length remainder + 1 in
    case degrees of
        45 -> (Just ((replicate prefix '_' ) ++ begin ++ (replicate suffix '_' ))) : transformGrid remainder 45
        135 -> (Just ((replicate suffix '_' ) ++ begin ++ (replicate prefix '_' ))) : transformGrid remainder 135

-- Resolves Maybes in the transformed grid
undiagonalized :: Grid -> Int -> Grid
undiagonalized grid degrees = case degrees of
    45 -> catMaybes $ transformGrid grid 45
    135 -> catMaybes $ transformGrid grid 135
