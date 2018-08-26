#!/usr/bin/env runhaskell 
{- https://projecteuler.net/problem=100
Problem 100
Arranged probability

If a box contains twenty-one coloured discs, composed of fifteen blue discs and six red discs, and two discs were taken at random, it can be seen that the probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.

The next such arrangement, for which there is exactly 50% chance of taking two blue discs at random, is a box containing eighty-five blue discs and thirty-five red discs.

By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in total, determine the number of blue discs that the box would contain.
-}

Alright, some definitions to begin with:

t = total discs
b = blue discs
r = red discs


Note that: 0 < b < t and r = t - b

We know that this should hold:

 1     b     b - 1
--- = --- * -------
 2     t     t - 1


Let's try to isolate b.


Applying basic algebra to the rhs of the above equation:

 1     b^2 - b
--- = ---------
 2     t^2 - t

Isolating numerator of rhs:

           t^2 - t
b^2 - b = ---------
              2 

          t ^ 2 - t
Let t' = ----------- and rearrange:
              2

b^2 - b - t' = 0

Apply the abc-formula (a' = 1, b' = -1, c' = -t') to isolate b:

     -b' +/- sqrt[b'^2 - 4a'c']
b = ------------------------
              2a'

     1 +/- sqrt[1 - -4 t']
  = -----------------------
              2

     1 +/- sqrt[1 + 4 t']
  = -----------------------
              2

The problem is now to find the smallest t > 10^12 that yields an integer solution when plugged into the equation above.


Let s = 1 + 4 t'

We're only interested in positive integer solutions for b.

Therefore it must be the case that:
 - s is a square (its sqrt must yield an integer)
 - s is an odd number (its sqrt must therefore also be odd)

                                t^2 - t
If we expand s we get: 1 + 4 * --------- = 2 t^2 - 2t + 1
                                   2

Let's see if we can find values of t:

> squares :: [Integer]
> squares = map (^ 2) [1,3..]

> t' :: Integer -> Integer
> t' t = 2 * t ^ 2 - 2 * t + 1

> t's :: [(Integer, Integer)]
> t's = [(t' t, t) | t <- [1..]]

> eqs :: (Ord a) => [a] -> [(a, b)] -> [b]
> eqs [] _ = []
> eqs _ [] = []
> eqs xss@(x:xs) yss@(y:ys)
>   | x == (fst y)    = (snd y) : eqs xs ys
>   | x < (fst y)     = eqs xs yss
>   | otherwise       = eqs xss ys

> sample :: [Integer]
> sample = take 8 $ eqs squares t's
> -- [1,4,21,120,697,4060,23661,137904,803761]

It appears we're onto something, the numbers 21 and 120 appear in the problem description.

The number 4 doesn't appear in the problem description but it does admit a solution.
Four total discs with 3 blue discs yields: (3/4)(2/3) = 0.5

Running the above computation until we've found a t > 10^12 would take too much time.
Let's instead try to discover the formula for the sequence:

1, 4, 21, 120, 697, 4060, 23661, 137904, 803761, ..

Let's try to fit a recurrence relation of the form:
x(n) = a x(n - 1) + b

where
x(1) =   1
x(2) =   4 = a * 1 + b
x(3) =  21 = a * 4 + b
x(4) = 120 = a * 21 + b
x(5) = 697 = a * 120 + b

No solutions.

Let's try to fit a recurrence relation of the form:
x(n) = a x(n - 2) + b x(n - 1) + c

x(1) =    1
x(2) =    4
x(3) =   21 = a       + b *   4 + c
x(4) =  120 = a *   4 + b *  21 + c
x(5) =  697 = a *  21 + b * 120 + c
x(6) = 4060 = a * 120 + b * 697 + c

a = -1, b = 6, c = -2

x(n) = -x(n - 2) + 6 x(n - 1) - 2

> x :: [Integer]
> x = 1 : 4 : 21 : zipWith (\n_2 n_1 -> -n_2 + 6 * n_1 - 2) x_2 x_1
>     where x_2 = tail x
>           x_1 = tail x_2

We can now find the first number of total discs above 10^12 that admits a solution:

> firstT :: Integer
> firstT = head $ dropWhile (< 10^12) x
> -- 1070379110497

Let's find out how many blue discs there are for this t using the formula for b that we derived earlier:

    1 + sqrt[2t^2 - 2t + 1]
b = -----------------------
              2

The answer is: 756872327473

> main :: IO ()
> main = putStrLn $ show 756872327473
