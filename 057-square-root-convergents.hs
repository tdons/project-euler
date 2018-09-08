#!/usr/bin/env stack
{- stack runghc --resolver lts-6.0 -}
{- https://projecteuler.net/problem=57
Problem 57
It is possible to show that the square root of two can be expressed as an infinite continued fraction.

√ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

By expanding this for the first four iterations, we get:

1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.

In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?
-}

import qualified Data.Ratio as DR
import           Data.Ratio ((%), Ratio)
import qualified Control.Applicative as CA (liftA2)

f :: (Integral a) => Ratio a -> Ratio a
f x = 1 / (2 + x)

numeratorHasMoreDigits :: (Show a, Integral a) => Ratio a -> Bool
numeratorHasMoreDigits = CA.liftA2 (>) (ls . DR.numerator) (ls . DR.denominator)
  where ls = length . show

main :: IO ()
main = print . length . filter numeratorHasMoreDigits . take 1000 . drop 1 . map (+ 1) . iterate f $ 0
