module Sandbox where

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving (Eq, Show)

type Side   = Float
type Radius = Float
type Vertex = (Float, Float)

sides                     :: Shape -> Int
sides (Rectangle _ _)     = 4
sides (RtTriangle _ _)    = 3
sides (Ellipse _ _)       = 42
sides (Polygon [])        = 0
sides (Polygon [_])       = 0
sides (Polygon [_, _])    = 0
sides (Polygon [_, _, _]) = 3
sides (Polygon (_:vs))    = 1 + sides (Polygon vs)

biggerVertice :: Float -> Vertex -> Vertex
biggerVertice e (x, y) = (x * sqrt e, y * sqrt e)

bigger                    :: Shape -> Float -> Shape
bigger (Rectangle l b) e  = Rectangle (l * sqrt e) (b * sqrt e)
bigger (RtTriangle a b) e = RtTriangle (a * sqrt e) (b * sqrt e)
bigger (Ellipse r1 r2) e  = RtTriangle (r1 * sqrt e) (r2 * sqrt e)
bigger (Polygon vl) e     = Polygon (map (biggerVertice e) vl)

minListNonRecursive :: [Int] -> Int
minListNonRecursive (x:xs) = foldr min x xs 

data Tree a = Leaf a | Branch (Tree a) (Tree a)
               deriving (Show, Eq)

-- `treeMap` is a version of `map` for Tree data type

treeMap                :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x)     = Leaf (f x)
treeMap f (Branch l r) = Branch (treeMap f l) (treeMap f r)

-- `treeFoldr` is a version of `foldr` for Tree data type

treeFoldr                            :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFoldr _       fLeaf (Leaf x)     =  fLeaf x
treeFoldr fBranch fLeaf (Branch l r) = fBranch (treeFoldr fBranch fLeaf l) (treeFoldr fBranch fLeaf r)

fringe :: Tree a -> [a] 
fringe = treeFoldr (++) (: [])

-- `treeSize` should return the number of leaves in the tree. 
-- So: `treeSize (Branch (Leaf 1) (Leaf 2))` should return `2`.

treeSize           :: Tree a -> Int
treeSize = treeFoldr (+) (\_ -> 1)

-- treeHeight :: Tree a -> Int
-- treeHeight = treeFoldr max (\_ -> 0)

moveDisc :: String -> String -> IO ()
moveDisc a b = putStrLn ("move disc from " ++ a ++ " to " ++ b)

hanoi :: Int -> String -> String -> String -> IO ()
hanoi 0 _ _ _ = putStr ("")
hanoi n a b c = do
    hanoi (n - 1) a c b
    moveDisc a b
    hanoi (n - 1) c b a

myMap          :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = foldr (\y ys -> (f y) : ys) [] (x:xs)

-- to test myMap
doubleList :: Num a => [a] -> [a]
doubleList = map (*2)

-- sierpinskiCarpet :: IO ()
-- sierpinskiCarpet = runGraphics (
--     do w <- openWindow "Sierpinski Carpet" (300, 300)
--        drawInWindow w (text (100,200) "Hello Graphics World") 
--        k <- getKey w
--        closeWindow w
--        )


-- > 

-- `treeSize` should return the height of the tree.
-- So: `height (Branch (Leaf 1) (Leaf 2))` should return `1`.

-- > treeHeight :: Tree a -> Int
-- > treeHeight = error "Define me!"

-- Now, a tree where the values live at the nodes not the leaf.

-- > data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a)
-- >                       deriving (Show, Eq)

-- `takeTree n t` should cut off the tree at depth `n`.
-- So `takeTree 1 (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
-- should return `IBranch 1 ILeaf ILeaf`.

-- > takeTree :: Int -> InternalTree a -> InternalTree a
-- > takeTree = error "Define me!"

-- `takeTreeWhile p t` should cut of the tree at the nodes that don't satisfy `p`.
-- So: `takeTreeWhile (< 3) (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
-- should return `(IBranch 1 (IBranch 2 ILeaf ILeaf) ILeaf)`.

-- > takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
-- > takeTreeWhile = error "Define me!"
 
-- Write the function map in terms of foldr:

-- > myMap :: (a -> b) -> [a] -> [b]
-- > myMap = error "Define me!"
