#!/usr/bin/env runhaskell 
{- https://projecteuler.net/problem=99
Problem 99
Largest exponential

Comparing two numbers written in index form like 2^11 and 3^7 is not difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.

However, confirming that 632382^518061 > 519432^525806 would be much more difficult, as both numbers contain over three million digits.

Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text file containing one thousand lines with a base/exponent pair on each line, determine which line number has the greatest numerical value.

NOTE: The first two lines in the file represent the numbers in the example given above.
-}

import qualified Data.List as DL (maximumBy)
import qualified Data.Ord as DO (comparing)

main :: IO ()
main = do pairs <- readFile "resources/p099_base_exp.txt"
          print . fst . DL.maximumBy (DO.comparing snd) $ zipWith (,) [1..] (computeExponents pairs)
          where computeExponents s = map (\(a, b) -> a ^ b) . map makePair . lines $ s
                makePair :: [Char] -> (Integer, Integer)
                makePair s = read $ "(" ++ s ++ ")"