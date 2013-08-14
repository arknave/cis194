xor :: [Bool] -> Bool
xor = foldl (\x y -> x /= y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- Optional
myFoldl :: (a -> b -> a) -> a -> [b] -> a
--I have no idea how to do this, I guess I'll come back to it later
myFoldl f base xs = foldl f base xs
