import Data.List
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a, b)]
hanoi x a b c = (hanoi (x-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (x-1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 n a b c d = minimumBy (\x y -> compare (length x) (length y)) $ map (\x -> hanoi4' n x a b c d) [1..((div n 2) + 1)]

hanoi4' :: Integer -> Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4' 0 _ _ _ _ _ = []
hanoi4' 1 _ a b _ _ = [(a, b)]
hanoi4' n k a b c d = (hanoi (k) a c d) ++ (hanoi (n-k) a b d) ++ (hanoi (k) c b a)
