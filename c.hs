containsMatch :: Eq a => [(a, a)] -> Bool
containsMatch xs = maximum[if(fst(x) == snd(x)) then True else False |x <- xs]