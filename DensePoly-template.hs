module DensePoly() where
import PolyClass
import Representation

instance Functor DensePoly where
    fmap f (P p) = P (map f p)

removeLeadingZeros :: (Eq a, Num a) => [a] -> [a]
removeLeadingZeros [] = []
removeLeadingZeros (x : xs)
    | x == 0 = removeLeadingZeros xs
    | otherwise = x : xs

removeTrailingZeros :: (Eq a, Num a) => [a] -> [a]
removeTrailingZeros p = removeLeadingZeros $ reverse p

instance Polynomial DensePoly where
    zeroP = P []
    constP x = P [x]
    varP = P [0, 1]
    evalP (P p) x = foldr (\a acc -> a + acc * x) 0 p
    shiftP n (P p) = P (removeTrailingZeros (replicate n 0 ++ p))
    degree (P p) = length (removeTrailingZeros p) - 1
    nullP (P p) = all (== 0) p

addArrays :: Num a => [a] -> [a] -> [a]
addArrays [] [] = []
addArrays x [] = x
addArrays [] x = x
addArrays (x : xs) (y : ys) = (x + y) : addArrays xs ys

subArrays :: Num a => [a] -> [a] -> [a]
subArrays [] [] = []
subArrays x [] = x
subArrays [] (x : xs) =  (-1 * x) : subArrays [] xs
subArrays (x : xs) (y : ys) = (x - y) : subArrays xs ys

mulArraysWithIndex :: Num a => Int -> [a] -> [a] -> [a]
mulArraysWithIndex _ _ [] = []
mulArraysWithIndex _ [] _ = []
mulArraysWithIndex i (x : xs) ys = addArrays firstArray (mulArraysWithIndex (i + 1) xs ys)
    where firstArray = replicate i 0 ++ map (* x) ys

mulArrays :: Num a => [a] -> [a] -> [a]
mulArrays = mulArraysWithIndex 0

instance (Eq a, Num a) => Num (DensePoly a) where
    P p1 + P p2 = P (removeTrailingZeros (addArrays p1 p2))
    P p1 - P p2 = P (removeTrailingZeros (subArrays p1 p2))
    abs = undefined
    signum = undefined
    fromInteger e = constP $ fromIntegral e
    P p1 * P p2 = P (removeTrailingZeros (mulArrays p1 p2))
    

-- |
-- >>> x^3 - 1 :: DensePoly Integer 
-- P {unP = [-1,0,0,1]}

-- | Num operations give canonical results:
-- >>> isCanonicalDP (sampleDP - sampleDP)
-- True
    
instance (Eq a, Num a) => Eq (DensePoly a) where
    P p1 == P p2 = removeTrailingZeros p1 == removeTrailingZeros p2

-- |
-- >>>  P [1,2] == P [1,2]
-- True

-- |
-- >>> fromInteger 0 == (zeroP :: DensePoly Int)
-- True

-- |
-- >>>  P [0,1] == P [1,0]
-- False

-- | Degree examples
-- >>> degree (zeroP :: DensePoly Int)
-- -1
-- >>> degree (constP 1 :: DensePoly Int)
-- 0
