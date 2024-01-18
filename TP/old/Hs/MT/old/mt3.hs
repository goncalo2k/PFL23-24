type Species = (String, Int)
type Zoo = [Species]

isEndangered :: Species -> Bool
isEndangered (spec,num)
    | num <= 100 = True
    | otherwise = False

updateSpecies :: Species -> Int -> Species
updateSpecies (spec, num) babies = (spec, num+babies)

filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] _ = []
filterSpecies (z:zs) func
    | func z = z : filterSpecies zs func
    | otherwise = filterSpecies zs func

countAnimals :: Zoo -> Int
countAnimals animals = sum (map (\(name,count) -> count) animals)

substring :: (Integral a) => String -> a -> a -> String
substring str start end = [str !! fromIntegral i | i <- [start..end]]

hasSubstr :: String -> String -> Bool
hasSubstr _ "" = True
hasSubstr "" _ = False
hasSubstr (x1:xs1) (x2:xs2) 
    | x1 == x2 = hasSubstr xs1 xs2
    | otherwise = hasSubstr xs1 (x2:xs2)

sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo)
sortSpeciesWithSubstr animals str = ([s| s <- animals, hasSubstr (fst s) str], [s| s <- animals, not (hasSubstr (fst s) str)])
    where end = length str - 1

rabbits :: (Integral a) => [a]
rabbits = 2 : 3 : zipWith (+) rabbits (tail rabbits)

rabbitYears :: (Integral a) => a -> Int
rabbitYears targetPopulation = length (takeWhile (< targetPopulation) rabbits)