#!/usr/bin/env stack
{- stack runghc --resolver lts-6.0 -} 
{- https://projecteuler.net/problem=62
Problem 62
Cubic permutations

The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.
-}

import Data.List as DL (groupBy, sortOn, sort)

cubes :: (Integral a) => [a]
cubes = map (^3) [1..]

groupByLength :: Show a => [a] -> [[a]]
groupByLength = DL.groupBy (\a b -> (length . show $ a) == (length . show $ b))

sortDigits :: Show a => [a] -> [[Char]]
sortDigits = map (DL.sort . show)

-- Sometimes, naming is difficult.
fn :: Show a => [a] -> [[(a, [Char])]]
fn xs = filter ((>= 5) . length)
      . DL.groupBy (\a b -> snd a == snd b)
      . DL.sortOn snd
      $ zipWith (,) xs (sortDigits xs)

main :: IO ()
main = print
     . head
     . DL.sort
     . map fst
     . head
     . concat
     . map fn
     . groupByLength
     $ cubes