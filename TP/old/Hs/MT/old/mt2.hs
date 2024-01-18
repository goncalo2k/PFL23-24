maxpos :: [Int] -> Int
maxpos [] = 0
maxpos (x:xs) = max x (maxpos xs)

dups :: [a] -> [a]
dups [] = []
dups [x] = [x,x]
dups (x:y:xs) = x : x : y : dups xs

transforma :: String -> String
transforma [] = []
transforma (x:xs)
    | x `elem` "aeiou" = x : 'p' : x : transforma xs
    | otherwise = x : transforma xs

type Vector = [Int]
type Matriz = [[Int]]

transposta :: Matriz -> Matriz
transposta [] = []
transposta m = [head x | x <- m ] : transposta [tail x | x <- m, tail x /= []]

prodInterno :: Vector -> Vector -> Int
prodInterno [a] [b] = a * b
prodInterno (a:af) (b:bf) = a*b + prodInterno af bf

--6 por fazer

data Arv a = F | N a (Arv a) (Arv a)
    deriving(Show)

alturas :: Arv a -> Arv Int
alturas F = F
alturas (N a a1 a2) = N (alturasAux (N a a1 a2)) (alturas a1) (alturas a2)

alturasAux :: Arv a -> Int
alturasAux F = 0
alturasAux (N a a1 a2) = 1 + max (alturasAux a1) (alturasAux a2)

--8 por fazer

f :: (a ->b ->c) -> b-> a -> c
f func a b = func b a 