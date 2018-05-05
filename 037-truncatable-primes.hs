#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=37
Problem 37
Truncatable primes

The number 3797 has an interesting property. Being prime itself, it is
possible to continuously remove digits from left to right, and remain
prime at each stage: 3797, 797, 97, and 7. Similarly we can work from
right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from
left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
-}

-- Very similar to 035-circular-primes.hs

import qualified Data.Numbers.Primes as DNP (isPrime, primes)
import qualified Data.List as DL (inits, tails)

truncations :: [a] -> [[a]]
truncations xs = (init $ DL.tails xs) ++ (tail $ DL.inits xs)

isTruncatablePrime :: Int -> Bool
isTruncatablePrime p = all (DNP.isPrime . read) . truncations . show $ p

main :: IO ()
main = print . sum . take 11 . filter isTruncatablePrime . filter (> 7) $ DNP.primes
