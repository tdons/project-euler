#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=56
Problem 56
Powerful digit sum

A googol (10^100) is a massive number: one followed by one-hundred zeros; 100^100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?
-}

import qualified Data.Char as DC (digitToInt)

digitSum :: Integer -> Int
digitSum x = sum . map DC.digitToInt $ show x

main :: IO ()
main = print $ maximum [digitSum $ a ^ b | a <- [1..100], b <- [1..100]]
