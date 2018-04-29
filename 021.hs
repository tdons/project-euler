#!/usr/bin/env stack
{- stack runghc --resolver lts-6.0 --package primes -} 
{- https://projecteuler.net/problem=21
Problem 21
Amicable numbers

Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n). If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
-}

import qualified Data.Numbers.Primes as DNP (primes)
import qualified Data.List as DL (intersect, nub, subsequences)

primeFactors :: (Integral a) => a -> [a]
primeFactors 1 = []
primeFactors n = factor : primeFactors (n `quot` factor)
   where factor = head . filter divides $ DNP.primes
         divides x = n `rem` x == 0

properDivisors :: (Integral a) => a -> [a]
properDivisors n = DL.nub . filter (< n) . map product . DL.subsequences . primeFactors $ n

main :: IO ()
main = print . sum . map fst $ DL.intersect pairs [(b, a) | (a, b) <- pairs]
    where pairs = filter (\(a, b) -> a /= b) [(a, sum . properDivisors $ a) | a <- [1..9999]]