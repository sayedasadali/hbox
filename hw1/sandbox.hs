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

fringe              :: Tree a -> [a]
-- fringe (Leaf x)     = [x]
-- fringe (Branch l r) = (fringe l) ++ (fringe r)

-- `treeSize` should return the number of leaves in the tree. 
-- So: `treeSize (Branch (Leaf 1) (Leaf 2))` should return `2`.

-- > treeSize           :: Tree a -> Int
-- > treeSize (Leaf x)  = x
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
