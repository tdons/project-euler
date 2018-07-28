#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=52
Problem 52
Permuted multiples

It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
-}

sameDigits :: Int -> Int -> Bool
sameDigits a b = all ((==) True . flip elem b') a' && length a' == length b'
  where a' = show a
        b' = show b

criterion :: Int -> Bool
criterion x = all (\m -> sameDigits x (x * m)) [2..6]

main :: IO ()
main = print . head $ filter criterion [1..]
