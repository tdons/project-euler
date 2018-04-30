#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=22
Problem 22
Names scores

Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
-}

import qualified Data.List as DL (sort)
import qualified Data.Char as DC (ord)

-- Ugly.
score :: String -> Int
score = sum . map (\c -> DC.ord c - DC.ord 'A' + 1)

scoreNames :: [String] -> Int
scoreNames names = sum . map (\(a, b) -> a * score b) $ sorted
     where sorted = zipWith ((,)) [1..] (DL.sort names)

main :: IO ()
main = do names <- readFile "resources/022/p022_names.txt"
          print . scoreNames . read $ "[" ++ names ++ "]"