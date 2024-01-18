--2.1
    --a)
mand :: [Bool] -> Bool
mand [True] = True
mand (x:xs) = x && mand xs

    --b)
mor :: [Bool] -> Bool 
mor [] = False
mor (x:xs) = x || mor xs

    --c)
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

    --d)
myReplicate :: Int -> a -> [a]
myReplicate 1 a = [a]
myReplicate n a = myReplicate (n-1) a ++ [a]

    --e)
heh :: [a] -> Int -> a
heh (x:_)  0 = x 
heh (_:xs) n
    | n < 0 = error "lol no"
    | otherwise = heh xs (n-1)

    --f)
myElem :: Eq a => a-> [a] -> Bool
myElem _ [] = False
myElem val (x:xs) = val == x || myElem val xs

--2.2
myIntersperse :: a -> [a] -> [a]
myIntersperse _ [] = []
myIntersperse _ [x] = [x]
myIntersperse c (x:xs) = x : c : myIntersperse c xs

--2.3
mdc :: Integer -> Integer -> Integer
mdc a b
    | b == 0 = a
    | otherwise = mdc b (a `mod` b)

--2.4
    --a)
myInsert :: Ord a => a -> [a] -> [a]
myInsert a [] = [a]
myInsert x (y:ys)
    | x <= y = x : y : ys
    | otherwise = y : myInsert x ys 

    --b)
myIsort :: Ord a => [a] -> [a]
myIsort [] = []
myIsort (x:xs) = myInsert x (myIsort xs)

--2.5
    --a)
myMinimum :: Ord a => [a] ->a
myMinimum [x] = x
myMinimum (x:xs) = min x (myMinimum (xs))
    --b)
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' val (x:xs)
    | val == x = xs
    | otherwise = x : delete' val xs

    --c)
ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort xs = myMinimum xs : ssort (delete' (myMinimum xs) xs)

--2.6
somaQuadrados = sum [x^2 | x <- [1..100]]
--2.7
    --a)
aprox :: Int -> Double
aprox n = 4 * sum[((-1)^k)/ (2 * fromIntegral k +1 ) | k <- [0..n]]
    --b)
aprox' :: Int -> Double
aprox' n = 12 * sqrt(sum[((-1)^k)/ (fromIntegral k + 1 )^2 | k <- [0..n]])

--2.8
dotprod :: [Float] -> [Float] -> Float
dotprod xs ys = sum [x*y | (x,y) <- zip xs ys]

--2.9
divprop :: Integer -> [Integer] 
divprop n = [x | x <- [1..n-1], n `mod` x == 0]

--2.10
perfeitos :: Integer -> [Integer]
perfeitos n = [x | x <- [1..n-1], sum(divprop x) == x]

--2.11
pitagoricos :: Integer -> [(Integer,Integer,Integer)]
pitagoricos n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2, z <=n]

--2.14
pascal :: Integer -> [[Integer]]
pascal n = []

binom :: Integer -> Integer -> Integer
binom n k = factorial n `div` (factorial k * factorial(n-k))

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)