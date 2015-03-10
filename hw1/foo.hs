module Foo where
--import SOE
 
--makeCircle (x,y) r = ellipse (x-r, y-r) (x+r, y+r)

--circles = reverse $ zipWith withColor colors bwcircles
--  where bwcircles = [makeCircle center (25*i) | i <- [1..(length colors)]]
--        colors    = [Red, Blue, Green, Cyan, Magenta, Yellow]
--        center    = (350, 225)  

--test = runGraphics $ do 
--  w <- openWindow "Test" (300,300)
--  drawInWindow w $ text (10,10) "Hello World" 
--  sequence_ $ map (drawInWindow w) circles
--  k <- getKey w
--  closeWindow w

average :: [Int] -> Int
average xs = sum xs `div` length xs

bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  

max' :: (Ord a) => a -> a -> a
max' a b
	| a > b 	= a
	| otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
	| a > b 	= GT
	| a == b 	= EQ
	| otherwise	= LT

myBMITell :: (RealFloat a) => a -> a -> String
myBMITell weight height 
	| bmi <= skinny	= "underweight"
	| bmi <= normal	= "normal"
	| bmi <= fatty 	= "fatty"
	| otherwise		= "pfft"
	where
		bmi = weight / (height * height)
		(skinny, normal, fatty) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials first last' = [f] ++ "." ++ [l] ++ "."
	where
		(f:_) = first
		(l:_) = last'

--maximum' :: (Ord a) <= a -> a
--maximum' []  	= error "empty"
--maximum' [x] 	= x
--maximum' (x:xs)
--	| x > maxTail = x
--	| otherwise = maxTail
--	where maxTail = maximum' xs 

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []		= []
filter' f (x:xs)
	| f x 		= x : filter' f xs
	| otherwise	= filter' f xs 
