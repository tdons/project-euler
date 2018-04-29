#!/usr/bin/env runhaskell
{-
Problem 9
Special Pythagorean triplet

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

isTriplet :: (Integral a) => (a, a, a) -> Bool
isTriplet (a, b, c) = a * a + b * b == c * c

triplets :: (Integral a) => [(a, a, a)]
triplets = [(a, b, t - a - b) | a <- [1..t], b <- [1..t], a > b, t - a - b > 0]
         where t = 1000

main :: IO ()
main = print (a * b * c)
     where (a, b, c) = head . filter isTriplet $ triplets
