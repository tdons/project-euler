#!/usr/bin/env stack
{- stack runghc --resolver lts-6.0 --package unordered-containers -} 
{- https://projecteuler.net/problem=39
Problem 39
Integer right triangles

If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?
-}

import qualified Data.HashSet as DHS (HashSet, fromList, member)
import qualified Data.List as DL (group, sort, sortBy)
import qualified Data.Ord as DO (comparing)

p :: Int
p = 1000

sqr :: Int -> Int
sqr = floor . sqrt . fromIntegral

squares :: DHS.HashSet Int
squares = DHS.fromList . map (^ 2) $ [1..p]

abcs :: [(Int, Int, Int)]
abcs = [(sqr $ c * c - b * b, b, c) | c <- [1..p], b <- [1..c], DHS.member (c * c - b * b) squares]

main :: IO ()
main = print . fst . head . DL.sortBy (flip $ DO.comparing snd) . map (\xs -> (head xs, length xs)) . DL.group . DL.sort $ [a + b + c | (a,b,c) <- abcs, a + b + c < p]
