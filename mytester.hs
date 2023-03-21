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
    | otherwise = (x : xs)

removeTrailingZeros :: (Eq a, Num a) => [a] -> [a]
removeTrailingZeros p = removeLeadingZeros $ reverse p

instance Polynomial DensePoly where
    zeroP = P []
    constP x = P [x]
    varP = P [0, 1]
    evalP (P p) x = foldr (\a acc -> a + acc * x) 0 p
    shiftP n (P p) = P (replicate n 0 ++ p)
    degree (P p) = (length $ removeTrailingZeros p) - 1
    nullP (P p) = all (== 0) p