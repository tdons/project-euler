#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=7
Problem 7
10001st prime

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
-}

isPrime :: Integer -> Bool
isPrime x = all (\p -> x `rem` p /= 0) $ takeWhile (<= x') primes
            where x' :: Integer 
                  x' = ceiling $ sqrt (fromInteger x)

primes :: [Integer]
primes = 2 : 3 : 5 : 7 : filter isPrime [8..]

main :: IO ()
main = print $ primes !! 10000
