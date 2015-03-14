> avg :: [Int] -> Int
> avg xs = sum xs `div` length xs

> main = print $ avg [5..10]
