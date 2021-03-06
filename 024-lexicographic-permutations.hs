#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=24
Problem 24
Lexicographic permutations

A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
-}

fac :: (Integral a) => a -> a
fac n = product [1..n]

nthPermutationOf :: (Eq a) => Int -> [a] -> [a]
nthPermutationOf _ [] = []
nthPermutationOf permutation xss = digit : nthPermutationOf (permutation - index * f) (filter (/= digit) xss)
   where f = fac $ length xss - 1
         index = permutation `quot` f
         digit = xss !! index

main :: IO ()
main = print answer 
   where answer = read . concat . map show $ nthPermutationOf (10 ^ 6 - 1) [0..9] :: Integer
