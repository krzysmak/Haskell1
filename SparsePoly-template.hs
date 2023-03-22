{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module SparsePoly(fromDP, toDP, qrP) where
import PolyClass
import Representation
import Data.List

-- | fromDP example
-- >>> fromDP sampleDP
-- S {unS = [(3,1),(0,-1)]}
fromDP :: (Eq a, Num a) => DensePoly a -> SparsePoly a
toDP :: (Eq a, Num a) => SparsePoly a -> DensePoly a

fromDP = undefined
toDP = undefined

first :: (a -> a') -> (a, b) -> (a', b)
first = undefined
second :: (b -> b') -> (a, b) -> (a, b')
second = undefined

simplify :: (Eq a, Num a) => [(Int, a)] -> [(Int, a)]
simplify [] = []
simplify ((x, y) : xs)
    | x == 0 = simplify xs
    | otherwise = (x, y) : simplify xs

sortAndSimplify :: (Eq a, Num a) => [(Int, a)] -> [(Int, a)]
sortAndSimplify xs = simplify $ sortBy (\(x, _) (y, _) -> compare x y) xs

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

instance (Eq a, Num a) => Num (SparsePoly a) where

instance (Eq a, Num a) => Eq (SparsePoly a) where
    p == q = nullP(p-q)

-- qrP s t | not(nullP t) = (q, r) iff s == q*t + r && degree r < degree t
qrP :: (Eq a, Fractional a) => SparsePoly a -> SparsePoly a -> (SparsePoly a, SparsePoly a)
qrP = undefined

-- | Division example
-- >>> qrP (x^2 - 1) (x -1) == ((x + 1), 0)
-- True
