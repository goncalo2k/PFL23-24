--3.1
--filter p (map f xs)
--3.2
dec2int :: [Int] -> Int
dec2int= foldl (\acc digit -> acc*10 + digit) 0

--3.4 
isort' :: Ord a => [a] -> [a]
isort' = foldl