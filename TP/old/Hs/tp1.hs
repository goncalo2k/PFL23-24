-- 1.1
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a + b > c) && (a + c >b) && (b + c > a)

-- 1.2 
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = 
    let
        s = (a + b + c) / 2
        area = sqrt(s * (s - a) * (s - b) * (s - c))
    in
        area

-- 1.3
metades :: [a] -> ([a],[a])
metades xs = (take metade xs, drop metade xs)
    where 
        metade = length xs `div` 2

--1.4 
    -- a)
ultimo :: [a] -> a
ultimo xs = 
    let 
        revList = reverse xs
        ultElem = head revList
    in
        ultElem

    -- b)

--1.5
    --a)
binom :: Integer -> Integer -> Integer
binom n k = (factorial n) `div` ((factorial k) * factorial(n-k))

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

    --b)

--1.7
    --a) List [Char]
    --b) tuple(Char)
    --c) List [tuple(Bool,Char)]
    --d) tuple(List [Bool], List [Char])
    --e) ??
    --f) ??

--1.9
classifica :: Int -> String
classifica x | x<=9 = "Reprovado"
             | x >= 10 && x <= 12 = "Suficiente"
             | x >= 13 && x <= 15 = "Bom"
             | x >= 16 && x <= 18 = "muito bom"
             | x >= 19 && x <= 20 = "muito bom com distinção"
