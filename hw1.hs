--Name : Yash Mahant
--Assignment #1

--Question 1: Warm-Up
--(a)
seqabcABC = "aAbBcC"

onlyABC :: [Char] -> Bool
onlyABC x = minimum[elem t seqabcABC | t <- x]

--(b)
countPerfectSquares :: [Int] -> Int
countPerfectSquares (xs) = foldl(\acc x ->  (if (floor(sqrt (fromIntegral x))*floor(sqrt (fromIntegral x)) == x ) then (acc + 1) else acc)) 0 xs

--(c)
containsMatch :: Eq a => [(a, a)] -> Bool
containsMatch xs = maximum[if(fst(x) == snd(x)) then True else False |x <- xs]

--Question 2: Discrete Math Flashback: Properties of Relations
f :: Eq a => [a] -> [a]
f = fBy (==)

fBy :: (a -> a -> Bool) -> [a] -> [a]
fBy eq [] = []
fBy eq (x:xs) = x : fBy eq (filter (\y -> not (eq x y)) xs)

domain :: Eq a => [(a,b)] -> [a]
domain a = f [x | (x , _) <- a]

range :: Eq b => [(a,b)] -> [b]
range a = f [y | (_ , y) <- a]

member x  [] = False
member x (y:ys)
    | x == y = True
    | otherwise = member x ys

--(a)
isSymmetric :: Eq a => [(a, a)] -> Bool
isSymmetric a = and [member (y, x) a | (x,y) <- a]
--(b)
isTransitive :: Eq a => [(a, a)] -> Bool
isTransitive a = and [member (x,z) a | (x,y) <- a, (y, z) <- a]
--(c)
isReflexive :: Eq a => [(a, a)] -> Bool
isReflexive a = and [member (x , x) a | x <- (domain a) ++ (range a)]
--(d)
isEquivalenceRelation :: Eq a => [(a,a)] -> Bool
isEquivalenceRelation x
    | isSymmetric x==True && isReflexive x ==True && isTransitive x ==True = True
    | otherwise = False

--Question 3: Perfection
isPerfect n = (n == sum [v | v <- [1..n-1], mod n v == 0])
perfectNumbers :: Int -> [Int]
perfectNumbers n = take n $ filter isPerfect [1..]

--Question 4: Prime Factors
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = f:(primeFactors $ div n f)
  where f = head $ filter (\x -> mod n x == 0) [2..n]

--Question 5 (Bonus): Finite Automata
--abaMachine = [(1, 'a', 2), (2, 'a', 3), (2, 'b', 2)]
--accept :: [(Integer, Char, Integer)] -> [Char] -> Bool
--evalDFA :: DFA st -> String -> Bool
--evalDFA (qs, sigma, delta, s, inF) w =
  --inF (deltaStar s w)
  --where deltaStar q [] = q
        --deltaStar q (a:w) = deltaStar (delta q a) w

-- I tried to play arounf with the bonus Question but was not able to simulate entire problem


--Question 6 (Required only for CS670
digitsRemove :: [Integer] -> Integer
digitsRemove xs = aux xs 0
  where aux [] sum = sum
        aux (x:xs) sum  = aux xs ((sum * 10) + x)

multSpecial :: [Integer] -> [Integer] -> Integer
multSpecial sx sy = digitsRemove sx * digitsRemove sy
