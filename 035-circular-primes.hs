#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=35
Problem 35
Circular primes

The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
-}

import qualified Data.Numbers.Primes as DNP (isPrime, primes)
import qualified Data.List as DL (inits, tails)

rotations :: [a] -> [[a]]
rotations xs = init $ zipWith (++) (DL.tails xs) (DL.inits xs)

isCircularPrime :: Int -> Bool
isCircularPrime p = all (DNP.isPrime . read) . rotations . show $ p

main :: IO ()
main = print . length . filter isCircularPrime . takeWhile (< 10 ^ 6 - 1) $ DNP.primes
