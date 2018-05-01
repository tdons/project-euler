#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=26
Problem 26
Reciprocal cycles

A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1
Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
-}


--
-- Example
-- 1/7 = 0.(142857)
-- 10^6 * 1/7 = 142857.142857
-- 10^6 * 1/7 - 1/7 = 142857
-- (10^6 - 1) * 1/7 = 142857
-- 1/7 = 142857 / (10^6 - 1)
--
-- For 1/d we look for an n such that
-- 10^n == 1 (mod d)
--

import qualified Data.List as DL (maximumBy)
import qualified Data.Ord as DO (comparing)


-- Call only if:
-- 1 < d ∧ 2 ∤ d ∧ 5 ∤ d
periodOfReciprocal :: (Integral a) => a -> a
periodOfReciprocal d = snd . head . filter ((== 1) . fst) $ zipWith (,) powers [1..]
  where powers = map (fm d . (10 ^)) [1..]
        fm = flip mod

main :: IO ()
main = print . fst . DL.maximumBy (DO.comparing snd) . map (\(a, b) -> (a, periodOfReciprocal b)) $ zipWith (,) candidates candidates
  where candidates = [x | x <- [3..999], x `mod` 2 /= 0, x `mod` 5 /= 0]
