#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=36
Problem 36
Double-base palindrome

The decimal number, 585 = 1001001001_2 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are  palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)
-}

import qualified Text.Printf as TP (printf)

palindrome :: (Eq a) => [a] -> Bool
palindrome n = n == (reverse n)

decimal :: Int -> String
decimal = show

binary :: Int -> String
binary = TP.printf "%b"

main :: IO ()
main = print . sum . filter (palindrome . binary) . filter (palindrome . decimal) $ candidates
  where candidates :: [Int]
        -- Filter everything divisable by 10 or 2 since they can't be palindromes (least significant digit is 0 in their respective bases)
        candidates = [n | n <- [1..(10 ^ 6 - 1)], n `rem` 2 /= 0, n `rem` 10 /= 0]
