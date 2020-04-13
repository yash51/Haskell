-- Simple Haskell implementation of the A* algorithm

import Data.List (sortBy)
import Data.List (sort)
import Data.Function (on)

-- Solution of the shuffle problem in Assignment #2
hwSolution :: [[Char]]
hwSolution = aStar "ULDO" "LOUD" h'Shuffle genStatesShuffle

-- Solution of another shuffle problem
anotherSolution :: [[Char]]
anotherSolution = aStar "DIRTYROOM" "DORMITORY" h'Shuffle genStatesShuffle

-- Use the A* algorithm to find an optimal path from a start state to a goal state based on a given h' function
-- (optimistically) estimating the remaining number of steps, and a genState function returning all states that can
-- be reached within one operation from the (given) current state. If no solution exists, aStar returns [],
-- otherwise the solution is a list of states from the start state to the goal state.
aStar :: Eq a => a -> a -> (a->a->Int) -> (a->[a]) -> [a]
aStar start goal h' genStates = expand [([start], h' start goal)]
    where expand [] = []
          expand ((path, score):nodes)
              | head path == goal       = reverse path
              | otherwise               = expand $ sortBy (compare `on` snd) (nodes ++ newNodes)
              where newNodes = [(state:path, length path + h' state goal) | state <- genStates $ head path, state `notElem` path]

-- Generate a list of all states that can be directly reached from the current state in the shuffle problem
genStatesShuffle :: [Char] -> [[Char]]
genStatesShuffle state = [take (n - 1) (tail state) ++ [head state] ++ drop n state | n <- [2..(length state)]]

-- Compute the h' function for the shuffle problem, estimating the number of required steps from the current state to
-- the goal state. This function is guaranteed to be optimistic and also works with repetitions of characters.
h'Shuffle :: [Char] -> [Char] -> Int
h'Shuffle curr goal = maximum [length $ takeWhile (/= curr!!c) (reverse $ take (c + 1) goal) | c <- [0..(length curr) - 1]]

--Q1)a part
solveShuffle :: [Char] -> [Char] -> [[Char]]
solveShuffle curr goal = if (sort curr == sort goal) then aStar curr goal h'Shuffle genStatesShuffle else [[]]


--Q1)b part
-- ideas that you have to limit the search we can use something like filter that can help A* limit its deepining searches
--solveShuffle :: [Char] -> [Char] -> [[Char]]
--h'Shuffle curr goal = filter [maximum [length $ takeWhile (/= curr!!c) (reverse $ take (c + 1) goal) | c <- [0..(length curr
