#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=40
Problem 40
Champernowne's constant

An irrational decimal fraction is created by concatenating the positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If d_n represents the nth digit of the fractional part, find the value of the following expression.

d_1 × d_{10} × d_{100} × d_{1000} × d_{10000} × d_{100000} × d_{1000000}
-}

import qualified Data.Char as DC (digitToInt)

champernownes :: [Char]
champernownes = concat . map show $ [1..]

indices :: [Int]
indices = map (+ (-1)) . map (10 ^) $ [0..6]

main :: IO ()
main = print . product . map DC.digitToInt $ zipWith (!!) (repeat champernownes) indices
