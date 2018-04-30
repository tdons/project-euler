#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=4
Problem 4
Largest palindrome product

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

isPalindrome :: (Integral a, Read a, Show a) => a -> Bool
isPalindrome a = read (reverse $ show a) == a

main :: IO ()
main = print . maximum $ [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]
