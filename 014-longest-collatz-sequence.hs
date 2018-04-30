#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=14
Problem 14
Longest Collatz sequence

The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains
10 terms. Although it has not been proved yet (Collatz Problem), it is thought
that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
-}

import qualified Data.List as DL (maximumBy)
import qualified Data.Ord as DO (comparing)

-- Note: pretty naive implementation.  To make sure the program runs in a reasonable time, compile it with ghc -O3

next :: (Integral a) => a -> a
next n
    | even n    = n `div` 2
    | otherwise = 3 * n + 1

collatzLength :: (Integral a) => a -> a
collatzLength 1 = 1
collatzLength n = 1 + collatzLength (next n)

main :: IO ()
main = print . fst . DL.maximumBy (DO.comparing snd) $ [(x, collatzLength x) | x <- [1..999999]]
