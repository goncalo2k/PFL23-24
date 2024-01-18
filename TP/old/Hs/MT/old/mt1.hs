maxpos :: [Int] -> Int
maxpos [] = 0
maxpos [x] = x
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
{-Nota: as matrizes são retangulares, ou seja, o
 comprimento de todas as sublistas é idêntico.-}

transposta :: Matriz -> Matriz
transposta [] = []
transposta ([]:_) = []
transposta  mat = map head mat : transposta (map tail mat)

prodInterno :: Vector -> Vector -> Int
prodInterno [] [] = 0
prodInterno (a:af) (b:bf) = a*b + prodInterno af bf

{-prodMat :: Matriz -> Matriz -> Matriz
prodMat mat1 mat2 = [[somaProduto i j | j <- transposta mat2] | i <- mat1]
    where somaProduto linha coluna = sum $ zipWith (*) linha coluna-}
