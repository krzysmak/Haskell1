class Polynomial p where
  zeroP  :: p a                           -- zero polynomial
  constP :: (Eq a, Num a) => a -> p a     -- constant polynomial
  varP   :: Num a => p a                  -- p(x) = x
  x      :: Num a => p a                  -- p(x) = x
  x       = varP
  evalP  :: Num a => p a -> a -> a        -- value of p(x) at given x
  shiftP :: (Eq a, Num a) => Int -> p a -> p a    -- multiply by x^n
  degree :: (Eq a, Num a) => p a -> Int   -- highest power with nonzero coefficient
  nullP  :: (Eq a, Num a) => p a -> Bool  -- True for zero polynomial
  nullP p = degree p < 0

newtype DensePoly a = P { unP :: [a] } deriving Show

instance Functor DensePoly where
    fmap f (P p) = P (map f p)

removeLeadingZeros :: (Eq a, Num a) => [a] -> [a]
removeLeadingZeros [] = []
removeLeadingZeros (x : xs)
    | x == 0 = removeLeadingZeros xs
    | otherwise = x : xs

removeTrailingZeros :: (Eq a, Num a) => [a] -> [a]
removeTrailingZeros p = reverse $ removeLeadingZeros $ reverse p

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
    
instance (Eq a, Num a) => Eq (DensePoly a) where
    P p1 == P p2 = removeTrailingZeros p1 == removeTrailingZeros p2