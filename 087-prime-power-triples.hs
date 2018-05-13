#!/usr/bin/env runhaskell 
{- https://projecteuler.net/problem=87
Problem 87
Prime power triples

The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28. In fact, there are exactly four numbers below fifty that can be expressed in such a way:

28 = 2^2 + 2^3 + 2^4
33 = 3^2 + 2^3 + 2^4
49 = 5^2 + 2^3 + 2^4
47 = 2^2 + 3^3 + 2^4

How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?
-}

import qualified Data.Numbers.Primes as DNP (primes)
import qualified Data.List as DL (sort, group)

upperBound :: Int
upperBound = 50 * 10 ^ 6

nPowers :: Int -> [Int]
nPowers n = takeWhile (< upperBound) . map (^ n) $ DNP.primes

nthPowers :: [[Int]]
nthPowers = map nPowers [4,3,2]

sums :: [Int] -> [Int] -> [Int]
sums (x:xs) yss = map (+ x) yss ++ sums xs yss
sums [] yss = []

uniq :: (Ord a) => [a] -> [a]
uniq = map head . DL.group . DL.sort

main :: IO ()
main = print . length . uniq . filter (< upperBound) . last . scanl1 sums $ nthPowers
