#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=79
Problem 79
Passcode derivation

A common security method used for online banking is to ask the user for three random characters from a passcode. For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.

The text file, keylog.txt, contains fifty successful login attempts.

Given that the three characters are always asked for in order, analyse the file so as to determine the shortest possible secret passcode of unknown length.
-}

import qualified Data.Set as DS
import qualified Data.Char as DC (digitToInt)
import qualified Data.Graph as DG

parse :: String -> DG.Graph
parse s = DG.buildG (0, 9) edges
          where edges = concat . map (toEdge . (map DC.digitToInt)) . lines $ s
                toEdge :: [Int] -> [(Int, Int)]
                toEdge (a:b:c:[]) = (a, b) : (b, c) : []

digits :: String -> DS.Set Int
digits s = DS.fromList . map DC.digitToInt $ filter (/= '\n') s

main :: IO ()
main = do attempts <- readFile "resources/079/p079_keylog.txt"
          print
            . concatDigits
            -- Filtering out the digits that aren't in the passphrase is kind of cheeky/ugly.
            . filter (flip DS.member (digits attempts))
            . DG.topSort
            . parse $ attempts
          where concatDigits :: [Int] -> Int
                concatDigits = read . concat . map show