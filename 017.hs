#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=17
Problem 17
Number letter counts

If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
-}

numberInWords :: (Integral a) => a -> String
numberInWords n
        | n < 0     = undefined
        | n > 9999  = undefined
        | n == 0    = "zero"
        | n == 1    = "one"
        | n == 2    = "two"
        | n == 3    = "three"
        | n == 4    = "four"
        | n == 5    = "five"
        | n == 6    = "six"
        | n == 7    = "seven"
        | n == 8    = "eight"
        | n == 9    = "nine"
        | n == 10   = "ten"
        | n == 11   = "eleven"
        | n == 12   = "twelve"
        | n == 13   = "thirteen"
        | n == 14   = "fourteen"
        | n == 15   = "fifteen"
        | n == 16   = "sixteen"
        | n == 17   = "seventeen"
        | n == 18   = "eighteen"
        | n == 19   = "nineteen"
        | n == 20   = "twenty"
        | n == 30   = "thirty"
        | n == 40   = "forty"     -- *not* "fourty"
        | n == 50   = "fifty"
        | n == 60   = "sixty"
        | n == 70   = "seventy"
        | n == 80   = "eighty"
        | n == 90   = "ninety"
        | n < 100   = numberInWords (10 * (n `div` 10)) ++ " " ++ numberInWords (n `rem` 10)
        | n < 1000  = numberInWords (n `div` 100) ++ " hundred" ++ lt1k (n `rem` 100)
        | n < 10000 = numberInWords (n `div` 1000) ++ " thousand" ++ lt10k (n `rem` 1000)
        where lt1k n'
                | n' == 0   = ""
                | otherwise = " and " ++ numberInWords (n' `rem` 100)
              lt10k n'
                | n' == 0   = ""
                | otherwise = " and " ++ numberInWords (n' `rem` 1000)

main :: IO ()
main = print . length . filter (/= ' ') . concat . map numberInWords $ [1..1000]
