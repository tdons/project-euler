#!/usr/bin/env runhaskell 
{- https://projecteuler.net/problem=63
Problem 63
Powerful digit counts

The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.

How many n-digit positive integers exist which are also an nth power?
-}

-- Because 10^x has (x + 1) digits we only need to check 1..10

-- The lowest value x for which 9^x has (x - 1) digits is 22
-- For x >= 22, no x-th power will have precisely x digits because [1..9]^x all have <= x-1 digits, and 10^x has x+1 digits.
-- This function returns 21.
upperBound :: Int
upperBound = length $ takeWhile (== True) $ [(== x) . length . show $ 9 ^ x | x <- [1..]]

main :: IO ()
main = print . length . concat $ [filter ((== e) . length . show) $ zipWith (^) [1..10] (repeat e) | e <- [1..upperBound]]
