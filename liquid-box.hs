average :: [Int] -> Int
average xs = sum xs `div` length xs

main :: IO ()
main = print $ average [5..10]
