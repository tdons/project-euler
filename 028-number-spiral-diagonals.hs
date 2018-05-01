#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=28
Problem 28
Number spiral diagonals

Starting with the number 1 and moving to the right in a clockwise
direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13
 It can be verified that the sum of both diagonals is 101.

What is the sum of both diagonals in a 1001 by 1001 spiral formed in the
same way?
-}

-- rad = radius (2 in the example above)
sumOfDiagonals :: Int -> Int
sumOfDiagonals rad = sum $ scanl (+) 1 [r * 2 | r <- [1..rad], _ <- [0..3]]

main :: IO ()
main = print . sumOfDiagonals $ 500