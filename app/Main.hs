module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Starting up application, here is the puzzle:"
    outputGrid
    putStrLn "Words to serach are the following:"
    outputSearchWords
    putStrLn "Words found in the puzzle are:"
    putStrLn $ unlines checkForWordsComplete
