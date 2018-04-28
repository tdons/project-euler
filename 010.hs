{-
Problem 10
Summation of primes

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

-- Borrowed this from Problem 7
isPrime x = all (\p -> x `mod` p /= 0) $ takeWhile (<= x') primes
            where x' :: Integer 
                  x' = ceiling $ sqrt (fromInteger x)
primes :: [Integer]
primes = 2 : 3 : 5 : 7 : filter isPrime [8..]


main :: IO ()
main = putStrLn . show $ sum . takeWhile (< 2000000) $ primes
