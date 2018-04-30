#!/usr/bin/env runhaskell 
{- https://projecteuler.net/problem=48
Problem 48
Self powers

The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-}

main :: IO ()
main = print last10digits
  where num = show . sum . take 1000 $ zipWith (^) [1..] [1..]
        last10digits = read . drop (length num - 10) $ num :: Int