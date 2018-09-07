#!/usr/bin/env stack 
{- stack runghc --resolver lts-6.0 -} 
{- https://projecteuler.net/problem=89
Problem 89
Roman numerals

For a number written in Roman numerals to be considered valid there are basic rules which must be followed. Even though the rules allow some numbers to be expressed in more than one way there is always a "best" way of writing a particular number.

For example, it would appear that there are at least six ways of writing the number sixteen:

IIIIIIIIIIIIIIII
VIIIIIIIIIII
VVIIIIII
XIIIIII
VVVI
XVI

However, according to the rules only XIIIIII and XVI are valid, and the last example is considered to be the most efficient, as it uses the least number of numerals.

The 11K text file, roman.txt (right click and 'Save Link/Target As...'), contains one thousand numbers written in valid, but not necessarily minimal, Roman numerals; see About... Roman Numerals for the definitive rules for this problem.

Find the number of characters saved by writing each of these in their minimal form.

Note: You can assume that all the Roman numerals in the file contain no more than four consecutive identical units.


About... Roman Numerals
=======================

How do you read and write Roman numerals?

Traditional Roman numerals are made up of the following denominations:

I = 1
V = 5
X = 10
L = 50
C = 100
D = 500
M = 1000

In order for a number written in Roman numerals to be considered valid there are three basic rules which must be followed.

Numerals must be arranged in descending order of size.
M, C, and X cannot be equalled or exceeded by smaller denominations.
D, L, and V can each only appear once.

For example, the number sixteen could be written as XVI or XIIIIII, with the first being the preferred form as it uses the least number of numerals. We could not write IIIIIIIIIIIIIIII because we are making X (ten) from smaller denominations, nor could we write VVVI because the second and third rule are being broken.

The "descending size" rule was introduced to allow the use of subtractive combinations. For example, four can be written IV because it is one before five. As the rule requires that the numerals be arranged in order of size it should be clear to a reader that the presence of a smaller numeral out of place, so to speak, was unambiguously to be subtracted from the following numeral rather than added.

For example, nineteen could be written XIX = X (ten) + IX (nine). Note also how the rule requires X (ten) be placed before IX (nine), and IXX would not be an acceptable configuration (descending size rule). Similarly, XVIV would be invalid because V can only appear once in a number.

Generally the Romans tried to use as few numerals as possible when displaying numbers. For this reason, XIX would be the preferred form of nineteen over other valid combinations, like XIIIIIIIII or XVIIII.

By mediaeval times it had become standard practice to avoid more than three consecutive identical numerals by taking advantage of the more compact subtractive combinations. That is, IV would be written instead of IIII, IX would be used instead of IIIIIIIII or VIIII, and so on.

In addition to the three rules given above, if subtractive combinations are used then the following four rules must be followed.

Only one I, X, and C can be used as the leading numeral in part of a subtractive pair.
I can only be placed before V and X.
X can only be placed before L and C.
C can only be placed before D and M.
Which means that IL would be considered to be an invalid way of writing forty-nine, and whereas XXXXIIIIIIIII, XXXXVIIII, XXXXIX, XLIIIIIIIII, XLVIIII, and XLIX are all quite legitimate, the latter is the preferred (minimal) form. However, minimal form was not a rule and there still remain in Rome many examples where economy of numerals has not been employed. For example, in the famous Colosseum the numerals above the forty-ninth entrance is written XXXXVIIII rather than XLIX.

It is also expected, but not required, that higher denominations should be used whenever possible; for example, V should be used in place of IIIII, L should be used in place of XXXXX, and D should be used in place of CCCCC. However, in the church of Sant'Agnese fuori le Mura (St Agnes' outside the walls), found in Rome, the date, MCCCCCCVI (1606), is written on the gilded and coffered wooden ceiling; I am sure that many would argue that it should have been written MDCVI.

So if we believe the adage, "when in Rome do as the Romans do," and we see how the Romans write numerals, then it clearly gives us much more freedom than many would care to admit.
-}

import qualified Data.List as DL (groupBy, elemIndex, sortBy)
import qualified Data.Maybe as DM (fromJust)
import qualified Data.Ord as DO (comparing)

data RomanDigit = I | V | X | L | C | D | M
     deriving (Show, Eq, Read, Ord, Enum, Bounded)

romanDigitValuations :: (Integral a) => [a]
romanDigitValuations = [1, 5, 10, 50, 100, 500, 1000]

legalSubtractions :: [[RomanDigit]]
legalSubtractions = [[I, V], [I, X], [X, L], [X, C], [C, D], [C, M]]

--

digitToInt :: (Integral a) => RomanDigit -> a 
digitToInt = (!!) romanDigitValuations . fromEnum

intToDigit :: (Integral a) => a -> RomanDigit
intToDigit = toEnum . DM.fromJust . (flip DL.elemIndex) romanDigitValuations

romanToInt :: (Integral a) => [Char] -> a 
romanToInt = sum . map (foldr1 (-) . reverse) . DL.groupBy (<) . map (digitToInt . read) . fmap return

-- legalRomanSequences == [(1,"I"),(4,"IV"),(5,"V"),(9,"IX"),(10,"X"),(40,"XL"),(50,"L"),(90,"XC"),(100,"C"),(400,"CD"),(500,"D"),(900,"CM"),(1000,"M")]
legalRomanSequences :: (Integral a) =>  [(a, String)]
legalRomanSequences = DL.sortBy (DO.comparing fst) . map ((\c -> (romanToInt c, c)) . concat . map show) $ romanDigits ++ legalSubtractions
  where romanDigits = fmap return $ enumFrom I

intToRoman :: (Integral a) => a -> [Char]
intToRoman n = intToRoman' n (reverse legalRomanSequences)
  where intToRoman' 0 _ = []
        intToRoman' n xss = let (n', romanDigits) = head . filter ((<= n) . fst) $ xss
                             in romanDigits ++ intToRoman' (n - n') xss

countSavings :: [Char] -> Int
countSavings x = originalLength - (length . intToRoman $ n)
  where originalLength = length x
        n = romanToInt x

main :: IO ()
main = do fileContents <- readFile "resources/p089_roman.txt"
          print . sum . map countSavings . lines $ fileContents

