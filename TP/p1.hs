-- 1.1
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a < b+c) && (b < a+c) && (c < a+b)

--1.2
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a + b +c)/2

--1.3
metades :: [a] -> ([a],[a])
metades a = (take l a, drop l a)
    where l = (length a) `div` 2

--1.4
--a)
last' :: [a] -> a
last' l = head (drop s l)
    where s = length l -1

last2' :: [a]-> a
last2' l = head (reverse l) 

--b)
init' :: [a] -> [a]
init' l = take s l 
    where s = length l-1

init2' :: [a] -> [a]
init2' l = reverse (tail (reverse l))

--1.5
--a)
binom :: Integer -> Integer -> Integer
binom n k = nf `div` (kf * f)
    where nf = product [1..n]
          kf = product [1..k]
          f = product [1..n-k]

--1.6
raizes :: Float -> Float -> Float -> (Float,Float)
raizes a b c = ((-b+sqrt(b*b - 4 * a * c)) / (2*a),(-b-sqrt(b*b - 4 * a * c))/(2*a))
