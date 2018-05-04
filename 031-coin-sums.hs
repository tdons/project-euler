#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=31
Problem 31
Coin sums

In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p). It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?
-}

coins :: [Int]
coins = [200, 100, 50, 20, 10, 5, 2, 1]

sums :: Int -> [Int] -> Int
sums 0 _ = 1
sums _ [1] = 1
sums _ [] = 0
sums n (x:xs) = sum $ map (\q -> sums (n - x * q) xs) [0..(n `quot` x)]

main :: IO ()
main = print $ sums 200 coins
