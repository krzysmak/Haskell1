{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use bimap" #-}
module SparsePoly(fromDP, toDP, qrP) where
import PolyClass
import Representation
import Data.List

-- | fromDP example
-- >>> fromDP sampleDP
-- S {unS = [(3,1),(0,-1)]}
fromDP :: (Eq a, Num a) => DensePoly a -> SparsePoly a
toDP :: (Eq a, Num a) => SparsePoly a -> DensePoly a

fromDPArrayWithIndex :: (Eq a, Num a) => Int -> [a] -> [(Int, a)]
fromDPArrayWithIndex _ [] = []
fromDPArrayWithIndex i (x : xs)
    | x == 0 = fromDPArrayWithIndex (i + 1) xs
    | otherwise = (i, x) : fromDPArrayWithIndex (i + 1) xs

fromDP (P p) = S $ fromDPArrayWithIndex 0 p

toDPArrayWithIndex :: (Eq a, Num a) => Int -> [(Int, a)] -> [a]
toDPArrayWithIndex _ [] = []
toDPArrayWithIndex i ((p, c) : xs)
    | i == p = c : toDPArrayWithIndex (i + 1) xs
    | otherwise = 0 : toDPArrayWithIndex (i + 1) ((p, c) : xs)

toDP (S s) = P $ toDPArrayWithIndex 0 s

first :: (a -> a') -> (a, b) -> (a', b)
first = undefined
second :: (b -> b') -> (a, b) -> (a, b')
second = undefined

simplify :: (Eq a, Num a) => [(Int, a)] -> [(Int, a)]
simplify [] = []
simplify ((x, y) : xs)
    | y == 0 = simplify xs
    | otherwise = (x, y) : simplify xs

sortAndSimplify :: (Eq a, Num a) => [(Int, a)] -> [(Int, a)]
sortAndSimplify xs = simplify $ sortBy (flip (\(x, _) (y, _) -> compare x y)) xs

instance Functor SparsePoly where
    fmap f (S s) = S $ fmap (\(p, c) -> (p, f c)) s

instance Polynomial SparsePoly where
    zeroP = S []
    constP x = S [(0, x)]
    varP = S [(1, 1)]
    evalP (S s) x = foldr (\(p, c) acc -> acc + c * (x^p)) 0 s
    shiftP n (S s) = S $ fmap (\(p, c) -> (p + n, c)) (sortAndSimplify s)
    degree (S s)
        | null sorted = -1
        | otherwise = x
        where
            sorted = sortAndSimplify s
            (x, _) = last sorted
    nullP (S s) = null $ sortAndSimplify s

addSparse :: (Eq a, Num a) => [(Int, a)] -> [(Int, a)] -> [(Int, a)]
addSparse s [] = s
addSparse [] s = s
addSparse ((p1, c1) : xs) ((p2, c2) : ys)
    | p1 > p2 = (p1, c1) : addSparse xs ((p2, c2) : ys)
    | p1 < p2 = (p2, c2) : addSparse ((p1, c1) : xs) ys
    | otherwise = (p1, c1 + c2) : addSparse xs ys

subSparse :: (Eq a, Num a) => [(Int, a)] -> [(Int, a)] -> [(Int, a)]
subSparse s [] = s
subSparse [] ((p, c) : xs) = (p,-1 * c) : subSparse [] xs
subSparse ((p1, c1) : xs) ((p2, c2) : ys)
    | p1 > p2 = (p1, c1) : subSparse xs ((p2, c2) : ys)
    | p1 < p2 = (p2,-1 * c2) : subSparse ((p1, c1) : xs) ys
    | otherwise = (p1, c1 - c2) : subSparse xs ys

mulSparse :: (Eq a, Num a) => [(Int, a)] -> [(Int, a)] -> [(Int, a)]
mulSparse s [] = []
mulSparse [] s = []
mulSparse ((p, c) : xs) ys = addSparse firstPoly (mulSparse xs ys)
    where firstPoly = fmap (\(x, y) -> (x + p, y * c)) ys


instance (Eq a, Num a) => Num (SparsePoly a) where
    S s1 + S s2 = S $ simplify (addSparse (sortAndSimplify s1) (sortAndSimplify s2))
    S s1 - S s2 = S $ simplify (subSparse (sortAndSimplify s1) (sortAndSimplify s2))
    S s1 * S s2 = S $ simplify (mulSparse (sortAndSimplify s1) (sortAndSimplify s2))
    abs = undefined
    signum = undefined
    fromInteger e = constP $ fromIntegral e

instance (Eq a, Num a) => Eq (SparsePoly a) where
    p == q = nullP (p-q)

qrPNonZeroSorted :: (Eq a, Fractional a) => [(Int, a)] -> [(Int, a)] -> [(Int, a)] -> ([(Int, a)], [(Int, a)])
qrPNonZeroSorted acc p@((p1, c1) : xs) q@((p2, c2) : ys)
    | p1 < p2 = (acc, p)
    | otherwise = qrPNonZeroSorted newAcc s q
        where
            div = c2 / c1
            powerDiff = p1 - p2
            newAcc = (powerDiff, div) : acc
            s = subSparse p (mulSparse [(powerDiff, div)] q)   


qrPSorted :: (Eq a, Fractional a) => SparsePoly a -> SparsePoly a -> (SparsePoly a, SparsePoly a)
qrPSorted _ (S []) = undefined
qrPSorted (S []) _ = (S [], S [])
qrPSorted (S s1) (S s2) = (S $ fst x, S $ snd x)
    where x = qrPNonZeroSorted [] s1 s2

-- qrP s t | not(nullP t) = (q, r) iff s == q*t + r && degree r < degree t
qrP :: (Eq a, Fractional a) => SparsePoly a -> SparsePoly a -> (SparsePoly a, SparsePoly a)
qrP (S s1) (S s2) = qrPSorted (S $ sortAndSimplify s1) (S $ sortAndSimplify s2)

-- | Division example
-- >>> qrP (x^2 - 1) (x -1) == ((x + 1), 0)
-- True
