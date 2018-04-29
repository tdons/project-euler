#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=16
Problem 16
Power digit sum

2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
-}

import qualified Data.Char as DC (digitToInt)

main :: IO ()
main = print . sum . map DC.digitToInt . show $ 2 ^ 1000