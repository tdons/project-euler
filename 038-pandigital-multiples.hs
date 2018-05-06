#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=38
Problem 38
Prime digit replacements

Take the number 192 and multiply it by each of 1, 2, and 3:

192 × 1 = 192
192 × 2 = 384
192 × 3 = 576

By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?
-}

--
-- Note: this implementation doesn't cover the whole search space
--
-- After toying around with the problem in ghci I had a hunch that n would equal 2, I gave it a shot and
-- it turned out to be correct.
--

import qualified Data.List as DL (permutations, sort)
import qualified Data.Set as DS (Set, fromList, member)

pandigitals :: DS.Set Int
pandigitals = DS.fromList . map read . DL.permutations $ ['1'..'9']

concatenatedProduct :: Int -> Int
concatenatedProduct n = read $ (show n) ++ (show $ n * 2)

main :: IO ()
main = print . head . reverse . DL.sort . filter (flip DS.member $ pandigitals) $ [concatenatedProduct n | n <- [5000..9876]]
