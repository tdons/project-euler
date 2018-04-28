{-
Problem 3
Largest prime factor

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

import Data.List ((\\))

-- Any prime factor will be <= this number
m :: Integer
m = ceiling (sqrt 600851475143)

-- Find all divisors of the number.
divisors :: [Integer]
divisors = [x | x <- [2..m], 600851475143 `mod` x == 0]

-- Filter out non-prime numbers
nonPrimeDivisors :: [Integer]
nonPrimeDivisors = [y | x <- divisors, y <- divisors, x < y,  y `mod` x == 0]

primes :: [Integer]
primes = divisors \\ nonPrimeDivisors

main :: IO ()
main = putStrLn . show $ foldl1 max primes
