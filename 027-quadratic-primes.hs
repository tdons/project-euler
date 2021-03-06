#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=27
Problem 27
Quadratic primes

Euler published the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive
values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) +
41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is
clearly divisible by 41.

Using computers, the incredible formula n² − 79n + 1601 was
discovered, which produces 80 primes for the consecutive values n = 0 to
79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n² + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n e.g. |11| = 11 and |−4| =
4 Find the product of the coefficients, a and b, for the quadratic
expression that produces the maximum number of primes for consecutive
values of n, starting with n = 0.
-}

import Data.Numbers.Primes as DNP (isPrime)
import Data.List as DL (maximumBy)
import Data.Ord as DO (comparing)

quadratic :: Int -> Int -> (Int -> Int)
quadratic a b n = n * n + a * n + b

numPrimes :: (Int -> Int) -> Int
numPrimes f = length . takeWhile isPrime . map f $ [0..]

main :: IO ()
main = print . snd . DL.maximumBy (DO.comparing fst) . map (\(a, b) -> (numPrimes a, b)) $ candidates
  where candidates = [(quadratic a b, a * b) | a <- [-999..999], b <- [-1000..1000]]
