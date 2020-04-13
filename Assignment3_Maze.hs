-- Using the A* algorithm to find the shortest path in a maze

import Data.List (sortBy)
import Data.Function (on)

simpleMaze :: [[Char]]
simpleMaze = ["#################",
              "    #           #",
              "#   #   #####   #",
              "#   #   #       #",
              "#   #   #   #####",
              "#       #       #",
              "#####   #   #   #",
              "#           #    ",
              "#################"]

giantMaze :: [[Char]]
giantMaze = ["#############################################################################",
             "#                   #                                   #           #       #",
             "#   #####   #########   #################   #########   #   #####   #   #   #",
             "#       #               #                   #               #       #   #    ",
             "#####   #################   #################################   #####   #   #",
             "#   #   #               #                                   #           #   #",
             "#   #   #   #########   #############################   #   #########   #   #",
             "#       #   #       #           #           #           #               #   #",
             "#####   #   #   #   #########   #   #########   #########   #############   #",
             "#       #   #   #   #   #   #   #               #           #               #",
             "#   #####   #   #   #   #   #   #############   #   #########   #############",
             "#   #       #   #   #   #   #               #   #   #           #           #",
             "#   #   #####   #   #   #   #############   #####   #   #####   #   #####   #",
             "#   #   #       #       #               #           #           #   #   #   #",
             "#   #   #####   #####################   #########################   #   #   #",
             "#   #       #   #                   #       #       #       #       #       #",
             "#   #####   #   #   #   #########   #   #   #   #   #   #   #   #############",
             "#   #       #       #       #   #       #       #       #   #               #",
             "#   #####   #########   #   #   #   #####################   #############   #",
             "#       #               #       #                       #               #   #",
             "#####   #################   #########################   #########   #   #   #",
             "#   #           #                       #           #           #   #   #   #",
             "#   #####   #   #################   #####   #####   #   #####   #   #   #   #",
             "#           #   #       #       #       #   #       #       #   #   #   #   #",
             "#   #########   #   #   #   #   #####   #   #   #####   #####   #   #   #   #",
             "#       #           #       #           #   #           #           #       #",
             "#   #####################   #############   #   #############   #############",
             "#   #               #           #           #   #           #               #",
             "#   #   #########   #   #########   #########   #########   #   #########   #",
             "#   #   #       #   #           #   #               #           #           #",
             "#####   #   #   #   #   #####   #   #   #############   #########   #########",
             "        #   #   #   #           #   #       #       #   #                   #",
             "#########   #####   #   #########   #####   #   #   #   #################   #",
             "#                   #   #       #   #       #   #   #       #               #",
             "#   #############   #####   #   #   #   #####   #   #####   #   #############",
             "#               #           #       #           #           #               #",
             "#############################################################################"]


-- Shortest path solution for simpleMaze
simpleMazePath :: [(Int, Int)]
simpleMazePath = [(1,0),(1,2),(2,2),(3,2),(4,2),(5,2),(5,4),(5,6),(6,6),(7,6),(7,8),(7,10),(6,10),(5,10),(5,12),(5,14),(6,14),(7,14),(7,16)]

-- Find the shortest path for a given maze and print the maze including the path
showMazeSolution :: [[Char]] -> IO ()
showMazeSolution maze = showMazeWithPath maze (findMazePath maze)

-- Check whether the maze has exactly two entrances, and if so, find the shortest path (list of coordinates) connecting them.
-- If there is no solution, return []
findMazePath :: [[Char]] -> [(Int, Int)]
findMazePath maze
    | (length entrances) /= 2 = []
    | otherwise = aStar (entrances!!0) (entrances!!1) h'Maze (genStatesMaze maze)
    where entrances = [(row, col) | row <- [1..(length maze) - 2], col <- [0, length (maze!!row) - 1], (maze!!row)!!col == ' ']

-- For a given maze and path, print the maze including the path
showMazeWithPath :: [[Char]] -> [(Int, Int)] -> IO ()
showMazeWithPath maze path = putStrLn $ concat $ [addPath row col | row <- [0..(length maze) - 1], col <- [0..(length (maze!!row)) - 1]]
    where addPath row col = if col == 0 then "\n" ++ newChar else newChar
            where newChar = if (row, col) `elem` path then "." else [(maze!!row)!!col]

-- Use the A* algorithm to find an optimal path from a start state to a goal state based on a given h' function
-- (optimistically) estimating the remaining number of steps, and a genState function returning all states that can
-- be reached within one operation from the (given) current state. If no solution exists, aStar returns [],
-- otherwise the solution as a list of states from the start state to the goal state.
aStar :: Eq a => a -> a -> (a->a->Int) -> (a->[a]) -> [a]
aStar start goal h' genStates = expand [([start], h' start goal)]
    where expand [] = []
          expand ((path, score):nodes)
              | head path == goal       = reverse path
              | otherwise               = expand $ sortBy (compare `on` snd) (nodes ++ newNodes)
              where newNodes = [(state:path, length path + h' state goal) | state <- genStates $ head path, state `notElem` path]



-- Compute the h' function for the maze problem, estimating the number of required steps from the current state (position)
-- to the goal state (position). As usual, this function must be optimistic.
h'Maze :: (Int, Int) -> (Int, Int) -> Int
h'Maze curr goal = abs(fst goal -fst curr) + abs(snd goal -snd curr)


genStatesMaze :: [[Char]] -> (Int, Int) -> [(Int,Int)]
genStatesMaze maze(row, col) = filter isValid[((row, col-2), (row, col+2), row-1, col),(row+1,col) ]
      where isValid (r,c) = r >=0 && r<(length maze) && c<(length $ head maze) && (maze!!r)!! c== ' '
