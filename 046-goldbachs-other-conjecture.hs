#!/usr/bin/env runhaskell 
{- https://projecteuler.net/problem=46
Problem 46
Goldbach's other conjecture

It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

9 = 7 + 2×1^2
15 = 7 + 2×2^2
21 = 3 + 2×3^2
25 = 7 + 2×3^2
27 = 19 + 2×2^2
33 = 31 + 2×1^2

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
-}

import qualified Data.Numbers.Primes as DNP (primes, isPrime)

isSquare :: Int -> Bool
isSquare n = elem n squares'
  where squares' = takeWhile (<= n) $ zipWith (*) [1..] [1..]

oddComposites :: [Int]
oddComposites = filter (not . DNP.isPrime) [3,5..]

emptyList :: [a] -> Bool
emptyList [] = True
emptyList (x:_) = False

canBeWrittenAsSumOfPrimeAndTwiceASquare :: Int -> Bool
canBeWrittenAsSumOfPrimeAndTwiceASquare n = not . emptyList . filter (\p -> isSquare $ (n - p) `quot` 2) . filter (\p -> even (n - p)) $ primes'
  where primes' = takeWhile (<= n) DNP.primes

main :: IO ()
main = print . head . filter (not . canBeWrittenAsSumOfPrimeAndTwiceASquare) $ oddComposites
