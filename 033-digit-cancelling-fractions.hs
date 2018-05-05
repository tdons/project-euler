#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=33
Problem 33
Digit cancelling fractions

The fraction 49/98 is a curious fraction, as an inexperienced
mathematician in attempting to simplify it may incorrectly believe that
49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction,
less than one in value, and containing two digits in the numerator and
denominator.

If the product of these four fractions is given in its lowest common
terms, find the value of the denominator.
-}

import qualified Data.Char as DC (digitToInt)
import qualified Data.Ratio as DR ((%), denominator)

-- Filter out everything that is divisable by 10.  Also filters out (22, 33, ...) by checking for divisability of 11
candidates :: [Int]
candidates = [x | x <- [11..99], x `rem` 10 /= 0, x `rem` 11 /= 0]

-- Works only for numbers with 2 digits
shareDigit :: Int -> Int -> Bool
shareDigit a b = shareDigit' a' b'
  where a' = show a
        b' = show b
        shareDigit' (a:b:[]) (c:d:[]) = a == c || a == d || b == c || b == d

removeCommonDigit :: (Int, Int) -> (Int, Int)
removeCommonDigit (a, b) = (\(a, b) -> (DC.digitToInt a, DC.digitToInt b)) $ removeCommonDigit' a' b'
  where a' = show a
        b' = show b
        removeCommonDigit' (a:b:[]) (c:d:[])
          | a == c = (b, d)
          | a == d = (b, c)
          | b == c = (a, d)
          | b == d = (a, c)

denominatorsFor :: Int -> [Int]
denominatorsFor numerator = filter (shareDigit numerator) . filter (> numerator) $ candidates

isEq :: (Integral a) => ((a, a), (a, a)) -> Bool
isEq ((a, b), (c, d)) = (DR.%) a b == (DR.%) c d

main :: IO ()
main = print . DR.denominator . product . map (uncurry (DR.%) . fst) . filter isEq . map (\c -> (c, removeCommonDigit c)) $ candidateFractions
  where candidateFractions :: [(Int, Int)]
        candidateFractions = concat . map (\c -> zipWith (,) (repeat c) (denominatorsFor c)) $ candidates
