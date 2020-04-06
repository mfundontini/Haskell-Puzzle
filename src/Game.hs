module Game (description) where

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

description :: String
description = "This is an interactive version of the puzzle on Lib.hs"
