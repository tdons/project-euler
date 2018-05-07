#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=42
Problem 42
Coded triangle numbers

The n^{th} term of the sequence of triangle numbers is given by, t_n = ½n(n+1); so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?
-}

triangles :: [Int]
triangles = map (\n -> n * (n + 1) `quot` 2) [1..]

letterValue :: Char -> Int
letterValue c = snd . head . filter ((c ==) . fst) $ lvs
  where lvs = zipWith (,) ['A','B'..'Z'] [1..]

isTriangle :: Int -> Bool
isTriangle x = (1 ==) . length . filter (== x) . takeWhile (<= x) $ triangles

isTriangleWord :: [Char] -> Bool
isTriangleWord w = isTriangle . sum . map letterValue $ w

main :: IO ()
main = do names <- readFile "resources/042/p042_words.txt"
          print . length . filter isTriangleWord . read $ "[" ++ names ++ "]"