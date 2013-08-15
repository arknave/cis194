{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib $ enumFrom 0

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)
          deriving (Eq)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s $ streamFromSeed f (f s)

nats :: Stream Integer
-- Generates the whole numbers [0..], the natural numbers are actually [1..]
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x $ interleaveStreams ys xs

ruler :: Stream Integer
-- Should generate 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0... etc
ruler = ruler' 0

ruler' :: Integer -> Stream Integer
ruler' n = interleaveStreams (streamRepeat n) (ruler' $ n+1)

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

scale :: Integer -> Stream Integer -> Stream Integer
scale n (Cons a as) = Cons (n*a) $ scale n as

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate s = scale (-1) s
  (+) (Cons a as) (Cons b bs) = Cons (a+b) $ as + bs
  (*) (Cons a as) bf@(Cons b bs) = Cons (a*b) $ (scale a bs) + as*bf

instance Fractional (Stream Integer) where
  -- TODO: Independently derive this
  (/) af@(Cons a as) bf@(Cons b bs) = Cons (a `div` b) $ scale (1 `div` b) (as - (af / bf)*bs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

data Matrix = Square Integer Integer Integer Integer
          deriving (Eq, Show)

getA :: Matrix -> Integer
getA (Square a b c d) = a

instance Num Matrix where
  fromInteger i = Square i i i i
  negate (Square a b c d) = Square (-a) (-b) (-c) (-d)
  (+) (Square a b c d) (Square f g h j) = Square (a+f) (b+g) (c+h) (d+j)
  (*) (Square a b c d) (Square f g h j) = Square (a*f+b*h) (a*g+b*j) (c*f+d*h) (c*g+d*j) 

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = getA $ (Square 1 1 1 0) ^ n
