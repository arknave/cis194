module Golf where

import Data.List

m :: (a -> b) -> [a] -> [b]
m = map

e :: (Eq a) => a -> a -> Bool
e = (==)

t :: Bool
t = True

o :: [a] -> Int
o = length

-- Given a list and an n, returns the list of every nth element.
q :: [a] -> Int -> [a]
q l x
  | o l <= x = [] 
  | t = (l !! x): q (drop (x + 1) l) x

-- Returns all of the skips
skips :: [a] -> [[a]]
skips l = m (q l) [0..(o l - 1)]

-- Filters out the local maxima from a list. First maps every value to a tuple
-- of its predecessor and successor, then filters out the tuples (a, b, c)
-- where b > a and b > c. Finally, extracts the middle elements from each of
-- them.
localMaxima :: [Integer] -> [Integer]
localMaxima n = m p $ filter l $ zip3 n (i n) $ i $ i n

i :: [Integer] -> [Integer]
i = tail

p :: (Integer, Integer, Integer) -> Integer
p (a, b, c) = b

l :: (Integer, Integer, Integer) -> Bool
l (a, b, c) = (b > a) && (b > c) 

-- This one's a doozy. 
-- m (c l) [0..9] creates a frequency map of the first 10 digits in the input
-- list. Look at the documentation of the d function to see how that works.
histogram :: [Integer] -> String
histogram l = d (m (c l) [0..9]) ++ "==========\n0123456789\n"

-- d is the draw function. Given a count of frequencies, it prints out a
-- histogram First, it computes the maximum value in the list. Then for every
-- element in the list which is equal to the maximum, print a star, otherwise a
-- space. Then recursively print the next row on the frequencies, with 1
-- subtracted from all of the maimums.
d :: [Integer] -> String
d l
  | e a 0 = ""
  | t = m p l ++ "\n" ++ (d $ m (n a) l)
  where
    p x  
      | e x a = '*' 
      | t = ' '
    a = maximum l

n :: Integer -> Integer -> Integer
n a x
  | e x a = a - 1 
  | t = x

c :: [Integer] -> Integer -> Integer
c [] _ = 0
c (y:ys) x
    | x == y = (+1) $ c ys x
    | t = c ys x
