#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=30
Problem 30

Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

1634 = 1^4 + 6^4 + 3^4 + 4^4
8208 = 8^4 + 2^4 + 0^4 + 8^4
9474 = 9^4 + 4^4 + 7^4 + 4^4

As 1 = 1^4 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
-}

-- A conservative upper bound is 1,000,000 since the maximum sum with
-- seven digits is 7 * 9^5 = 413,343 << 1,000,000

import qualified Data.Char as DC (digitToInt)

s :: Int -> Int
s = sum . map (^ 5) . map DC.digitToInt . show

main :: IO ()
main = print . sum . map fst . filter (\(a, b) -> a == b) . map (\x -> (x, s x)) $ [2..1000000] 
