import Distribution.Simple.Utils (xargs)
import Language.Haskell.TH (Strict)
maxpos :: [Int] -> Int
maxpos [] = 0
maxpos (x:xs)
    | x > maxpos xs = x
    | otherwise = maxpos xs

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
transposta ([]:_) = []
transposta mat = map head mat : transposta (map tail mat)

prodInterno :: Vector -> Vector -> Int
prodInterno [] [] = 0
prodInterno (a:as) (b:bs) = a*b + prodInterno as bs

prodMat :: Matriz -> Matriz -> Matriz
prodMat mat1 mat2 = [[sum (zipWith (*) row col)| col <- transposta mat2] | row <- mat1]

data Arv a = F | N a (Arv a) (Arv a)
    deriving(Show)

alturas :: Arv a -> Arv Int
alturas F = F
alturas (N a xs ys) = N (alturasAux (N a xs ys)) (alturas xs) (alturas ys)

alturasAux :: Arv a -> Int
alturasAux F = 0
alturasAux (N a no1 no2) = 1 + max (alturasAux no1) (alturasAux no2)

equilibrada :: Arv a -> Bool
equilibrada F = True
equilibrada (N a no1 no2) = (al1+1 == al2) || (al1 == al2+1) || (al1 == al2)
    where 
        al1 = alturasAux no1
        al2 = alturasAux no2
