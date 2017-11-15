
-- takes each number n in the argument list and generates n copies of it in the result list.
themselvesTimes :: [Int] -> [Int]
themselvesTimes ns = ns >>= \n -> replicate n n

--fs <*> xs = concatMap (\f -> map f xs) fs

-- (<*>) :: t (a -> b) -> t a -> t b
-- Maps each function a -> b in fs over each element of xs and concatenates all the results into a single list

apList :: [a -> b] -> [a] -> [b]
apList fs xs = [ f x | x <- xs, f <- fs ]