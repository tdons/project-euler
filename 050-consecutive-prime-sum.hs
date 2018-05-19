#!/usr/bin/env runhaskell 
{- https://projecteuler.net/problem=50
Problem 50
Consecutive prime sum

The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
-}

import Data.Numbers.Primes as DNP (primes, isPrime)
import Data.List as DL (sortBy, tails)
import Data.Ord as DO (comparing)

upperBound :: Int
upperBound = 10 ^ 6

-- Find longest prefix of ps that sums to a prime below upperBound
-- and returns (prefix length, prime)
f :: [Int] -> (Int, Int)
f ps = last . DL.sortBy (DO.comparing snd) $ ps'
  where sums = takeWhile ((< upperBound) . snd) $ zipWith (,) [1..] (scanl1 (+) ps)
        ps' = filter (DNP.isPrime . snd) $ sums

-- Does quite a lof of double work.
main :: IO ()
main = print . snd . last . DL.sortBy (DO.comparing fst) . map f . init . DL.tails . takeWhile (< upperBound) $ DNP.primes
