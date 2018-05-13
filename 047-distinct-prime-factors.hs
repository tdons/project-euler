#!/usr/bin/env runhaskell 
{- https://projecteuler.net/problem=47
Problem 47
Distinct primes factors

The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?
-}

import qualified Data.Numbers.Primes as DNP (primeFactors)
import qualified Data.List as DL (nub)

distinctFactors :: [(Int, Int)]
distinctFactors = zipWith (,) [1..] $ map (length . DL.nub . DNP.primeFactors) [1..]

fourConsecutives :: [(Int, Int)] -> Int
fourConsecutives ((x, 4):(_, 4):(_, 4):(_, 4):_) = x
fourConsecutives (x:xs) = fourConsecutives xs

main :: IO ()
main = print . fourConsecutives $ distinctFactors
