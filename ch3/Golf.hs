module Golf where

import Data.List

skips :: [a] -> [[a]]
skips l = map (q l) [0..(length l - 1)]

q :: [a] -> Int -> [a]
q [] _ = []
q l x
  | length l <= x = []
  | otherwise = (l !! x): q (drop (x+1) l) x

localMaxima :: [Integer] -> [Integer]
localMaxima n = map p $ filter l $ zip3 n (tail n) $ tail $ tail n

p :: (Integer, Integer, Integer) -> Integer
p (a,b,c) = b

l :: (Integer, Integer, Integer) -> Bool
l (a,b,c) = (b > a) && (b > c) 

histogram :: [Integer] -> String
histogram l = draw (map (count l) [0..9]) ++ "==========\n0123456789\n"

draw :: [Integer] -> String
draw l
  | m == 0 = ""
  | otherwise = map (\x->if x==m then '*' else ' ') l ++ "\n" ++ (draw $ map (\x-> if x==m then m-1 else x) l)
  where
    m = maximum l

count :: [Integer] -> Integer -> Integer
count [] _ = 0
count (y:ys) x
  | x==y = (+1) $ count ys x
  | otherwise = count ys x
