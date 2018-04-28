{-
Problem 4
Largest palindrome product

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

twoDigits :: [Int]
twoDigits = [100..999]

isPalindrome :: Int -> Bool
isPalindrome x = x' == reverse x'
                 where x' :: [Char]
                       x' = show x

main :: IO ()
main = putStrLn . show $ foldl1 max [x * y | x <- twoDigits, y <- twoDigits, isPalindrome (x * y)]
