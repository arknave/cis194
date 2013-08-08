toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | n < 10    = [n]
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | n < 10    = [n]
  | otherwise = [n `mod` 10] ++ toDigitsRev (n `div` 10)

--Doubles every other number in the list, starting from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []     = []
doubleEveryOther [x]    = [2*x]
doubleEveryOther (x:(y:ys))
  | ((length ys) `mod` 2) == 0 = [x, 2*y] ++ doubleEveryOther ys
  | otherwise                  = [2*x, y] ++ doubleEveryOther ys

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = (+) (sum $ toDigits x) (sumDigits xs)

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0
