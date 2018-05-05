#!/usr/bin/env runhaskell 
{- https://projecteuler.net/problem=53
Problem 53
Combinatoric selections

There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation, C^5_3 = 10.

In general,

         n!
nCr =	-------- , where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
      r!(n−r)!

It is not until n = 23, that a value exceeds one-million: C^{23}_{10} = 1144066.

How many, not necessarily distinct, values of  C^n_r, for 1 ≤ n ≤ 100, are greater than one-million?
-}

main :: IO ()
main = print . length . filter (> 10 ^ 6) $ [ncr n r | n <- [1..100], r <- [1..n]]
  where ncr n r = (fac n) `quot` ((fac r) * (fac (n - r)))
        fac n = product [1..n]