#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=43
Problem 43
Sub-string divisibility

The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d_1 be the 1^{st} digit, d_2 be the 2^{nd} digit, and so on. In this way, we note the following:

d_2 d_3 d_4    = 406 is divisible by 2
d_3 d_4 d_5    = 063 is divisible by 3
d_4 d_5 d_6    = 635 is divisible by 5
d_5 d_6 d_7    = 357 is divisible by 7
d_6 d_7 d_8    = 572 is divisible by 11
d_7 d_8 d_9    = 728 is divisible by 13
d_8 d_9 d_{10} = 289 is divisible by 17
Find the sum of all 0 to 9 pandigital numbers with this property.
-}

import qualified Data.List as DL (sortBy, permutations, inits, tails)

pandigitals :: [Int]
pandigitals = map read . DL.permutations $ ['0'..'9']

primes :: [Int]
primes = [2, 3, 5, 7, 11, 13, 17]

substrings :: Int -> [Int]
substrings = map read . take 7 . map (take 3) . tail . DL.tails . show

hasProperty :: Int -> Bool
hasProperty n = all (== 0) $ zipWith rem (substrings n) primes

main :: IO ()
main = print . sum . filter hasProperty $ pandigitals
