import Data.List.Split

valid :: String -> Bool
valid "" = False
valid s = not $ foldr (\a b -> b || a `elem` ["::", "import", "->", "module" ]) False $ words s

count :: String -> Int
count s = if valid s then length . concat $ words s else 0

chunky :: [Int] -> [Int] -> [Int]
chunky _ [] = []
chunky [] l = [sum l]
chunky (c:cs) l = (sum . take c $ l) : chunky cs (drop c l)

main = do
    file <- readFile "Golf.hs"
    mapM_ print $ chunky [2, 1, 4, 4] . map count . filter (valid) . lines $ file
