#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=41
Problem 41
Pandigital prime

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?
-}

import qualified Data.Numbers.Primes as DNP (isPrime)
import qualified Data.List as DL (permutations, inits, sortBy)

-- All pandigital numbers sorted in descending order
pandigitals :: [Int]
pandigitals = DL.sortBy (flip compare) . map read . concat . map DL.permutations . tail . DL.inits $ ['1'..'9']

main :: IO ()
main = print . head . filter DNP.isPrime $ pandigitals
