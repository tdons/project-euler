#!/usr/bin/env stack
{- stack runghc --resolver lts-6.0 --package primes -} 
{- https://projecteuler.net/problem=58
Problem 58
Spiral Primes

Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?
-}

import qualified Data.Numbers.Primes as DNP (isPrime)
import qualified Data.Ratio as DR

-- [3,5,7,9,13,17,21,25,31,37,43,49,..]
cornersOfRing :: [Integer]
cornersOfRing = drop 1 . scanl (+) 1 . concat . map (replicate 4) $ [2,4..]

-- Clunky.
--
-- > take 5 sideLengthsWithRatios
-- [(3,3 % 5),(5,5 % 9),(7,8 % 13),(9,9 % 17),(11,10 % 21)]
sideLengthsWithRatios :: [(Integer, DR.Ratio Integer)]
sideLengthsWithRatios = zipWith (,) [3,5..] ratios
  where ratios = corners . drop 3 . map (uncurry (DR.%)) . drop 1 . scanl f (0, 1) $ isPrimeCorner 
        isPrimeCorner :: [(Integer, Integer)]
        isPrimeCorner = map (\c -> (if DNP.isPrime c then 1 else 0, c)) cornersOfRing
        f (a, b) (c, d) = (a + c, b + 1) -- sum fst, snd = [1..]
        corners :: [a] -> [a]
        corners (a:b:c:d:e) = a : corners e

main :: IO ()
main = print . fst . head . filter ((< 1 DR.% 10) . snd) $ sideLengthsWithRatios
