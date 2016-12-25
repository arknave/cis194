module Golf where

import Data.List

m :: (a -> b) -> [a] -> [b]
m = map

e :: (Eq a) => a -> a -> Bool
e = (==)

t :: Bool
t = True

skips :: [a] -> [[a]]
skips l = m (q l) [0..(length l - 1)]

q :: [a] -> Int -> [a]
q l x
  | length l <= x = [] 
  | t = (l !! x): q (drop (x+1) l) x

localMaxima :: [Integer] -> [Integer]
localMaxima n = m p $ filter l $ zip3 n (i n) $ i $ i n

i :: [Integer] -> [Integer]
i = tail

p :: (Integer, Integer, Integer) -> Integer
p (a,b,c) = b

l :: (Integer, Integer, Integer) -> Bool
l (a,b,c) = (b > a) && (b > c) 

histogram :: [Integer] -> String
histogram l = d (m (c l) [0..9]) ++ "==========\n0123456789\n"

d :: [Integer] -> String
d l
  | e a 0 = ""
  | t = map p l ++ "\n" ++ (d $ m (n a) l)
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
