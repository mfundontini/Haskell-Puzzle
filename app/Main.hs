module Main where

import Lib
import Game

main :: IO ()
main = do
    putStrLn "Starting up application, here is the puzzle:"
    outputGrid
    putStrLn "Words to serach are the following:"
    outputSearchWords
    putStrLn "Words found in the puzzle are:"
    putStrLn $ unlines checkForWordsComplete
    putStrLn description
    putStrLn "Now using the Game.hs module with many more functions:"
    putStrLn "First let us build a screen from the previous Grid"
    outputAnyGrid $ createScreen
    -- or createParametrizedGrid Cell createCordinates grid
    putStrLn "Now prettfying this Bitch!"
    outputAnyGrid $ prettifyCellGrid $ createScreen
