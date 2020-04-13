-- Exports
module Game
        ( areMultipleWordsInCellGrid,
          checkForWordsCellGrid,
          convertCell,
          createArbitrayCordinates,
          createCordinates,
          createLimitedScreen,
          createParametrizedGrid,
          createScreen,
          createScreenSimplified,
          description,
          findWordInCellRow,
          isWordInCellGrid,
          isWordInReverse,
          outputAnyGrid,
          prettifyCellGrid,
          transformCellGrid,
          undiagonalizedCellGrid,
        ) where

-- Nested iteration [[(column, row) | column <- [0..7]]| row <- [0..7]] read as for each row, do this with the columns
-- Or  map (\s -> zip ([0..7]) s) (map (take 10 . repeat) [i | i <- [0..7]]) which is more complicated
-- They both return:
--[
--	[(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0)],
--	[(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1)],
--	[(0,2),(1,2),(2,2),(3,2),(4,2),(5,2),(6,2),(7,2)],
--	[(0,3),(1,3),(2,3),(3,3),(4,3),(5,3),(6,3),(7,3)],
--	[(0,4),(1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,4)],
--	[(0,5),(1,5),(2,5),(3,5),(4,5),(5,5),(6,5),(7,5)],
--	[(0,6),(1,6),(2,6),(3,6),(4,6),(5,6),(6,6),(7,6)],
--	[(0,7),(1,7),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7)]
--]

import Data.List
import Data.Maybe (catMaybes)
import Const (grid, Cell (Cell), Cordinates, Grid, ParametrizedGrid, Point, Row, Screen)

-- Make a string description for this module
description :: String
description = "This is an interactive version of the puzzle on Lib.hs"


-- Output any Grid constructly in a pretty manner
outputAnyGrid :: Show a => [a] -> IO ()
outputAnyGrid = putStrLn . unlines . map show


-- Create a square of cordinates X by X side big
createArbitrayCordinates :: Int -> Cordinates
createArbitrayCordinates 0 = [[(0,0)]]
createArbitrayCordinates side = map (\s -> zip ([0..side - 1]) s) (map (take side . repeat) [i | i <- [0..side - 1]])


-- Create a screen of X by X size with `grid` superimposed
createLimitedScreen :: Int -> ParametrizedGrid Cell
createLimitedScreen side = let
                rows = map (take side . repeat) [0..]
                columns = (take side . repeat) [0..] in
                    zipWith (zipWith Cell) (zipWith zip columns rows) (grid)


-- Create a screen only bound by the size of the `grid`
createScreen :: ParametrizedGrid Cell
createScreen = let
                rows = map (repeat) [0..]
                columns = (repeat) [0..] in
                    zipWith (zipWith Cell) (zipWith zip columns rows) (grid)


-- In order to go generic we need to break up the functions so we can understand their type
-- signatures better, create cordinates create a list of [(Int, Int)] which we call cordinates
-- they are essentially cells but I have kept the old code as a learning curve
-- createCordinates :: [[(Int, Int)]]
createCordinates :: Cordinates
createCordinates = let
            rows = map repeat [0..]
            columns = repeat [0..] in
                zipWith zip columns rows


-- createScreenSimplified is a composition of a createCordinates type function and a grid create function
-- where a grid is a list of strings, note that since coords and someGrid appear on both sides, they can
-- be removed, but left here for learning
-- Cordinates -> Grid -> Screen
createScreenSimplified :: ParametrizedGrid (Int, Int) -> ParametrizedGrid Char -> ParametrizedGrid Cell
createScreenSimplified coords someGrid = zipWith (zipWith Cell) coords someGrid


-- Generic parametrized grid creation
createParametrizedGrid :: (a -> b -> c) -> ParametrizedGrid a -> ParametrizedGrid b -> ParametrizedGrid c
createParametrizedGrid = zipWith . zipWith

-- return Char dimension of a cell
convertCell :: Cell -> Char
convertCell (Cell _ character) = character

prettifyCellGrid :: ParametrizedGrid Cell -> Grid
prettifyCellGrid [[]] = [""]
prettifyCellGrid cells = map (map (\s -> convertCell s)) cells


-- Check if the word is in a normalized grid, then reverse the word and check again
isWordInCellGrid :: String -> Grid -> Maybe String
isWordInCellGrid _ [] = Nothing
isWordInCellGrid word (begin:remainder) = if  word `isInfixOf` begin || ( (reverse word) `isInfixOf` begin) then Just word else isWordInCellGrid word remainder


-- Check the reverse of the word is in the CellGrid instead of the grid, this is not used
isWordInReverse :: String -> Grid -> Maybe String
isWordInReverse word grid = isWordInCellGrid (reverse word) grid

-- Call isWord in grid multiple times using a list of words
areMultipleWordsInCellGrid :: Grid -> [String] -> [String]
areMultipleWordsInCellGrid [[]] [] = []
areMultipleWordsInCellGrid grid list = catMaybes $ map (\s -> isWordInCellGrid s grid) list


-- Generic word searcher
checkForWordsCellGrid :: ParametrizedGrid Cell -> [String] -> [String]
checkForWordsCellGrid [[]] [] = []
checkForWordsCellGrid someCellGrid searchWords = let
                                              normalizedGrid = (prettifyCellGrid someCellGrid) in
                                                (areMultipleWordsInCellGrid normalizedGrid searchWords) ++
                                                (areMultipleWordsInCellGrid (transpose normalizedGrid) searchWords) ++
                                                (areMultipleWordsInCellGrid (transpose (undiagonalizedCellGrid normalizedGrid 45)) searchWords) ++
                                                (areMultipleWordsInCellGrid (transpose (undiagonalizedCellGrid normalizedGrid 135)) searchWords)


-- Coverts 45 and 135 degree diagonals to vertical
transformCellGrid :: Grid -> Int -> [Maybe String]
transformCellGrid [] _ = [Nothing]
transformCellGrid (begin:remainder) degrees = let
    offset = (length begin) - 1
    prefix = offset - ((length remainder) + 1)
    suffix = length remainder + 1 in
    case degrees of
        45 -> (Just ((replicate prefix '_' ) ++ begin ++ (replicate suffix '_' ))) : transformCellGrid remainder 45
        135 -> (Just ((replicate suffix '_' ) ++ begin ++ (replicate prefix '_' ))) : transformCellGrid remainder 135

-- Resolves Maybes in the transformed grid
undiagonalizedCellGrid :: Grid -> Int -> Grid
undiagonalizedCellGrid grid degrees = case degrees of
    45 -> catMaybes $ transformCellGrid grid 45
    135 -> catMaybes $ transformCellGrid grid 135

findWordInCellRow :: [Cell] -> [Char]-> [Cell] -> Maybe [Cell]
findWordInCellRow acm (currentChar:nextChars) (currentCell:nextCells) |
                                                    currentChar == (convertCell currentCell) = findWordInCellRow (acm ++ (currentCell:[])) nextChars nextCells |
                                                    currentChar /= (convertCell currentCell) = findWordInCellRow  acm (currentChar:nextChars) nextCells
findWordInCellRow acm [] _ = Just acm
findWordInCellRow _ _ _ = Nothing

findWordInCellGrid :: ParametrizedGrid Cell -> [Char] -> [[Cell]]
findWordInCellGrid [[]] [] = []
findWordInCellGrid cellGrid word = catMaybes $ map (\s -> (findWordInCellRow [] word s) ) cellGrid


findMultipleWordsInCellGrid :: ParametrizedGrid Cell -> [String] -> [[Cell]]
findMultipleWordsInCellGrid [[]] [] = []
findMultipleWordsInCellGrid cellGrid searchWords = map (\s -> findWordInCellGrid cellGrid s) searchWords


-- Generic word searcher
--checkForWordsCellGrid :: ParametrizedGrid Cell -> [String] -> [String]
--checkForWordsCellGrid [] [] = []
--checkForWordsCellGrid someCellGrid searchWords = let
--                                              normalizedGrid = (prettifyCellGrid someCellGrid) in
--                                                (areMultipleWordsInCellGrid normalizedGrid searchWords) ++
--                                                (areMultipleWordsInCellGrid (transpose normalizedGrid) searchWords) ++
--                                                (areMultipleWordsInCellGrid (transpose (undiagonalizedCellGrid normalizedGrid 45)) searchWords) ++
--                                                (areMultipleWordsInCellGrid (transpose (undiagonalizedCellGrid normalizedGrid 135)) searchWords)



