{-
Problem 5
Smallest multiple

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

-- I did this manually, easier than writing a program, here's how:
-- 
-- Divisors to consider: [1..20]
-- Primes in this list: [2,3,5,7,11,13,17,19]
-- 
-- Decomposition of the non-primes in the list:
-- 20 = 2 ^ 2         * 5
-- 18 = 2     * 3 ^ 2
-- 16 = 2 ^ 4
-- 15 =         3     * 5
-- 14 = 2                 * 7
-- 12 = 2 ^ 2 * 3
-- 10 = 2             * 5
--  9 =         3 ^ 2
--  8 = 2 ^ 3
--  6 = 2     * 3
--  4 = 2 ^ 2
--
-- To summarize: 2 ^ 4 * 3 ^ 2 * 5 * 7 * 11 * 13 * 17 * 19

main :: IO ()
main = putStrLn . show $ foldl1 (*) [2,2,2,2,3,3,5,7,11,13,17,19]
