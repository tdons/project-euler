#!/usr/bin/env runhaskell 
{- https://projecteuler.net/problem=92
Problem 92
Square digit chains

A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.

For example,

44 → 32 → 13 → 10 → 1 → 1
85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

How many starting numbers below ten million will arrive at 89?
-}

import qualified Data.Char as DC (digitToInt)

next :: Int -> Int
next = sum . map ((\d -> d * d) . DC.digitToInt) . show

endsIn89 :: Int -> Bool
endsIn89 = (== 89) . head . dropWhile (\d -> d /= 1 && d /= 89) . iterate next

main :: IO ()
main = print . length . filter (== True) . map endsIn89 $ [1..9999999]
