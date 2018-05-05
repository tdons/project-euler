#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=34
Problem 34
Digit factorials

145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
-}

-- Theoretical upper bound since the maximum sum you can create with ..
-- .. 8 digits is at most 8*9! = 2,903,040 which has only 7 digits (so 8 digits is out of the question)
-- .. 7 digits is at most 7*9! = 2,540,160 which does have 7 digits
bound :: Int
bound = 2540160

--
-- Hard coding these factorials makes things run quite a bit faster, I had this initially but it took ages:
-- digitFac c = product [1..(read $ c:[])]
--
digitFac :: Char -> Int
digitFac '0' = 1
digitFac '1' = 1
digitFac '2' = 2
digitFac '3' = 6
digitFac '4' = 24
digitFac '5' = 120
digitFac '6' = 720
digitFac '7' = 5040
digitFac '8' = 40320
digitFac '9' = 362880

main :: IO ()
main = print . sum . filter (\n -> n == (sum . map digitFac . show $ n)) $ [3..bound]
