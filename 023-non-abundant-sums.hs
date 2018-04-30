#!/usr/bin/env stack
{- stack runghc --resolver lts-6.0 --package primes --package unordered-containers -} 
{- https://projecteuler.net/problem=23
Problem 23
Non-abundant sums

A perfect number is a number for which the sum of its proper divisors is
exactly equal to the number. For example, the sum of the proper divisors of 28
would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n
and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123 can
be written as the sum of two abundant numbers. However, this upper limit cannot
be reduced any further by analysis even though it is known that the greatest
number that cannot be expressed as the sum of two abundant numbers is less than
this limit.

Find the sum of all the positive integers which cannot be written as the sum of
two abundant numbers.
-}

import qualified Data.Numbers.Primes as DNP (primeFactors)
import qualified Data.List as DL (nub, subsequences, group)
import qualified Data.HashSet as DHS

sumOfProperDivisors :: (Integral a) => a -> a
sumOfProperDivisors n = sumOfProperDivisors' n - n
    where sumOfProperDivisors' = product . map ((+ 1) . sum . zipWith (flip (^)) [1..]) . DL.group . DNP.primeFactors

-- All abundant numbers.
abundantNumbers :: (Integral a) => [a]
abundantNumbers = map fst . filter (\(num, divisorsum) -> num < divisorsum) $ pairs
   where pairs = [(x, sumOfProperDivisors x) | x <- [1..]]

-- All integers greater than 28123 can be written as the sum of two abundant numbers
upperBound :: (Integral a) => a
upperBound = 28123

abundantNumbers' :: DHS.HashSet Int
abundantNumbers' = DHS.fromList . takeWhile (< upperBound) $ abundantNumbers

hasAbundantNumberSum :: Int -> Bool
hasAbundantNumberSum n = any (\x -> DHS.member (n - x) abundantNumbers') (DHS.toList abundantNumbers')

haveNoAbundantSum :: [Int]
haveNoAbundantSum = filter (not . hasAbundantNumberSum) [1..upperBound]

main :: IO ()
main = print . sum $ haveNoAbundantSum
