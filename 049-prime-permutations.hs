#!/usr/bin/env runhaskell 
{- https://projecteuler.net/problem=49
Problem 49
Prime permutations

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
-}

-- The digit 0 cannot occur in a candidate prime of 4 digits since, because we need three different permutations,
-- the digit must be either the first digit in one (in which case it's no longer a 4-digit prime) or the last
-- digit, in which case it's no longer a prime.

import qualified Data.Numbers.Primes as DNP (primes)
import qualified Data.List as DL (group, sort, permutations, intersect, tails)

perms :: Int -> [Int]
perms = uniq . map read . DL.permutations . show

uniq :: (Ord a) => [a] -> [a]
uniq = map head . DL.group . DL.sort

-- List of lists, each inner list contains primes that are permutations of one another, for instance, this is one of the inner lists:
-- [1487,1847,4817,4871,7481,7841,8147,8741]
--
-- No inner list shares a prime with another inner list
--
-- length candidates = 143
candidates :: [[Int]]
candidates = (disjoints []) . filter ((>= 3) . length) . map (\x -> filter (flip elem $ primesWithoutZeroes) $ perms x) $ primesWithoutZeroes
  where disjoints xss (y:ys) = if (any (/= []) . map (DL.intersect y) $ xss) then disjoints xss ys else disjoints (xss ++ [y]) ys
        disjoints xss [] = xss
        primesWithoutZeroes = filter (not . (elem '0') . show) . takeWhile (<= 9999) . dropWhile (< 1000) $ DNP.primes

findIncreasingSequence :: [Int] -> [Int]
findIncreasingSequence (x:xs) = case (/= []) d'' of
  True  -> [x, fst . head $ d'', snd . head $ d'']
  False -> []
  where
    d = map (flip (-) $ x) $ xs
    d' = zipWith (,) xs (zipWith (+) xs d)
    d'' = filter (\e -> elem (snd e) xs) d'

main :: IO ()
main = print answer
  where answer :: Int
        answer = read . concat . map show . head . filter (/= [1487, 4817, 8147]) . filter (/= []) . map findIncreasingSequence . filter (/= []) . concat . map DL.tails $ candidates
