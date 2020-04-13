countPerfectSquares :: [Int] -> Int
countPerfectSquares (xs) = foldl(\acc x ->  (if (floor(sqrt (fromIntegral x))*floor(sqrt (fromIntegral x)) == x ) then (acc + 1) else acc)) 0 xs
