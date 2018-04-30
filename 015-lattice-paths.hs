#!/usr/bin/env stack
{- stack runghc --resolver lts-6.0 --package data-memocombinators -} 
{- https://projecteuler.net/problem=15
Problem 15
Lattice paths

Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?
-}

import qualified Data.MemoCombinators as DM (memo2, integral)

numRoutesInGrid :: (Integral a) => (a -> a -> a) -> a -> a -> a
numRoutesInGrid fn n m
        | n + m == 0 = 1
        | n == 0     = m'
        | m == 0     = n'
        | otherwise  = n' + m'
        where n' = fn (n - 1) m
              m' = fn n (m - 1)

memoizedNumRoutesInGrid :: (Integral a) => a -> a -> a
memoizedNumRoutesInGrid = DM.memo2 DM.integral DM.integral (numRoutesInGrid memoizedNumRoutesInGrid)

main :: IO ()
main = print $ memoizedNumRoutesInGrid 20 20