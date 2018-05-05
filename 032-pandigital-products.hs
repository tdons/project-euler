#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=32
Problem 32
Pandigital products

The product 7254 is unusual, as the identity, 39 × 186 = 7254,
containing multiplicand, multiplier, and product is 1 through 9
pandigital.

Find the sum of all products whose multiplicand/multiplier/product
identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to
only include it once in your sum.
-}

import qualified Data.List as DL (nub)

uniqueElements :: (Eq a) => [a] -> Bool
uniqueElements xs = (==) (length xs) (length . DL.nub $ xs)

isCandidate :: (Integral a, Show a) => a -> Bool
isCandidate c = (not . elem '0' . show $ c) && (uniqueElements . show $ c)

candidateProducts :: [Int]
candidateProducts = filter isCandidate [1000..9999]

candidateDivisors :: [Int]
candidateDivisors = filter isCandidate [2..99]

isPandigitalProduct :: (Integral a, Show a) => (a, a) -> Bool
isPandigitalProduct (product, divisor) = product `rem` divisor == 0 && isCandidate divisor' && length digits == 9 && uniqueElements digits
  where divisor' = product `quot` divisor
        digits = (show product ++ show divisor ++ show divisor')

main :: IO ()
main = print . sum . DL.nub $ [c | c <- candidateProducts, d <- candidateDivisors, isPandigitalProduct (c, d)]
