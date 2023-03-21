module DensePoly() where
import PolyClass
import Representation

instance Functor DensePoly where
    fmap f (P p) = P (map f p)

removeLeadingZeros :: Num a => [a] -> [a]
removeLeadingZeros [] = []
removeLeadingZeros (x : xs)
    | x == 0 = removeLeadingZeros xs
    | otherwise = (x : xs)

removeTrailingZeros :: Num a => [a] -> [a]
removeTrailingZeros p = removeLeadingZeros $ reverse p

instance Polynomial DensePoly where
    zeroP = P []
    constP x = P [x]
    varP = P [0, 1]
    evalP (P p) x = foldr (\a acc -> a + acc * x) 0 p
    shiftP n (P p) = P (removeTrailingZeros (replicate n 0 ++ p))
    degree (P p) = (length $ removeTrailingZeros p) - 1
    nullP (P p) = all (== 0) p

instance (Eq a, Num a) => Num (DensePoly a) where


-- |
-- >>> x^3 - 1 :: DensePoly Integer 
-- P {unP = [-1,0,0,1]}

-- | Num operations give canonical results:
-- >>> isCanonicalDP (sampleDP - sampleDP)
-- True
    
instance (Eq a, Num a) => Eq (DensePoly a) where

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
