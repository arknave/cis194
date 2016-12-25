import Data.List.Split

valid :: String -> Bool
valid "" = False
valid s = not $ foldr (\a b -> b || a `elem` ["::", "import", "->", "module" ]) False $ words s

count :: String -> Int
count s = if valid s then length . concat $ words s else 0

main = do
    file <- readFile "Golf.hs"
    let validLines = filter (valid) . lines $ file
    print validLines
    let lineLengths = map count validLines
    print lineLengths
    print (sum lineLengths)
