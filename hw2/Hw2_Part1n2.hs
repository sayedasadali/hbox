{-# LANGUAGE TypeSynonymInstances #-}
module Hw2_Part1n2 where

-- extra imports for unit testing
import Test.HUnit

-- Problem 1: All About `foldl`
-- ============================

-- Define the following functions by filling in the "error" portion:

-- 1. Describe `foldl` and give an implementation:

myFoldl                :: (a -> b -> a) -> a -> [b] -> a
myFoldl _  base []     = base
myFoldl op base (x:xs) = myFoldl op (op base x) xs

-- fails suspiciously with this implementation
-- myFoldl op base (x:xs) = (myFoldl op base xs) `op` x

-- 2. Using the standard `foldl` (not `myFoldl`), define the list reverse function:

myReverse :: [a] -> [a]
myReverse = foldl (\ys y -> y:ys) []

-- alternate method
-- myReverse = foldl (flip (:)) []

-- 3. Define `foldr` in terms of `foldl`:

myFoldr        :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b xs = foldl (\g c x -> g (f c x)) (\i -> i) xs b

-- 4. Define `foldl` in terms of the standard `foldr` (not `myFoldr`):

myFoldl2        :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f b xs = foldr (\c g x -> g (f x c)) (\i -> i) xs b
-- link:
-- https://wiki.haskell.org/Foldl_as_foldr_alternative
-- https://wiki.haskell.org/Foldl_as_foldr

-- 5. Try applying `foldl` to a gigantic list. Why is it so slow?
--    Try using `foldl'` (from [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#3))
--    instead; can you explain why it's faster?
-- A: good explanation here: https://hackhands.com/lazy-evaluation-works-haskell/

-- Part 2: Binary Search Trees
-- ===========================

-- Recall the following type of binary search trees:

data BST k v = Emp 
             | Bind k v (BST k v) (BST k v) 
             deriving (Show, Eq)

-- Define a `delete` function for BSTs of this type:

delete       :: (Ord k) => k -> BST k v -> BST k v
delete _ Emp = Emp
delete x (Bind k v l r)
       | x == k = deleteX (Bind k v l r)
       | x <  k = Bind k v (delete x l) r
       | x >  k = Bind k v l            (delete x r)

deleteX :: (Ord k) => BST k v -> BST k v
deleteX (Bind _ _ Emp r  ) = r
deleteX (Bind _ _ l   Emp) = l
deleteX (Bind _ _ l   r  ) = Bind k' v' l (delete k' r)
        where (k', v') = leftestNode r

leftestNode :: (Ord k) => BST k v -> (k, v)
leftestNode (Bind k v Emp _) = (k, v)
leftestNode (Bind _ _ l   _) = leftestNode l

-- End of Part 1 and 2
-- ===================

-- dummy trees for testing scenarios
-- =================================
-- TC : delete node with no child
tNoChild :: Num k => BST k [Char]
tNoChild = Bind 5 "5" 
                (Bind 2 "2"   (Bind (-4) "-4" Emp Emp) 
                              (Bind 3    "3"  Emp Emp)
                )
                (Bind 18 "18" Emp Emp)
-- result : delete -4
tNoChild' :: Num k => BST k [Char]
tNoChild' = Bind 5 "5" 
                (Bind 2 "2"   Emp
                              (Bind 3 "3" Emp Emp)
                )
                (Bind 18 "18" Emp Emp)

-- TC : delete node with 1 child
t1Child :: Num k => BST k [Char]
t1Child = Bind 5 "5" 
                (Bind 2 "2"   (Bind (-4) "-4" Emp Emp) 
                              (Bind 3    "3"  Emp Emp)
                )
                (Bind 18 "18" Emp
                              (Bind 21   "21" (Bind 19 "19" Emp Emp) 
                                              (Bind 25 "25" Emp Emp)
                              )
                )
-- result : delete 18 
t1Child' :: Num k => BST k [Char]
t1Child' = Bind 5 "5"
                (Bind 2 "2"   (Bind (-4) "-4" Emp Emp) 
                              (Bind 3 "3"     Emp Emp)
                )
                (Bind 21 "21" (Bind 19   "19" Emp Emp) 
                              (Bind 25   "25" Emp Emp)
                )

-- TC : delete node with 2 child
t2Child :: Num k => BST k [Char]
t2Child = Bind 5 "5" 
                (Bind 2 "2"   (Bind (-4) "-4" Emp Emp) 
                              (Bind 3    "3"  Emp Emp)
                )
                (Bind 12 "12" (Bind 9    "9"  Emp Emp)
                              (Bind 21   "21" (Bind 19 "19" Emp Emp) 
                                              (Bind 25 "25" Emp Emp)
                              )
                )

-- result : delete 
t2Child' :: Num k => BST k [Char]
t2Child' = Bind 5 "5" 
                (Bind 2 "2"   (Bind (-4) "-4" Emp Emp) 
                              (Bind 3    "3"  Emp Emp)
                )
                (Bind 19 "19" (Bind 9 "9"     Emp Emp)
                              (Bind 21 "21"   Emp
                                              (Bind 25 "25" Emp Emp)
                              )
                )

-- Test cases [source](https://hackage.haskell.org/package/HUnit-1.2.2.1/docs/Test-HUnit.html)
-- sample borrowed from:
-- http://zvon.org/other/haskell/Outputprelude/foldr_f.html
-- http://zvon.org/other/haskell/Outputprelude/foldl_f.html
tests :: Test
tests = test [ "test01" ~: "div"   ~: (2.0)            ~=? (myFoldl (/) 64 [4,2,4]),
               "test02" ~: "three" ~: (3.0)            ~=? (myFoldl (/) 3 []),
               "test03" ~: "max5"  ~: (5)              ~=? (myFoldl max 5 [1,2,3,4]),
               "test04" ~: "max7"  ~: (7)              ~=? (myFoldl max 5 [1,2,3,4,5,6,7]),
               "test05" ~: "2x+y"  ~: (43)             ~=? (myFoldl (\x y -> 2*x + y) 4 [1,2,3]),
               "test06" ~: "rev1"  ~: [4,3,2,1]        ~=? (myReverse [1,2,3,4]),
               "test07" ~: "rev2"  ~: reverse [1..100] ~=? (myReverse [1..100]),
               "test08" ~: "div"   ~: (2.0)            ~=? (myFoldl2 (/) 64 [4,2,4]),
               "test09" ~: "three" ~: (3.0)            ~=? (myFoldl2 (/) 3 []),
               "test10" ~: "max5"  ~: (5)              ~=? (myFoldl2 max 5 [1,2,3,4]),
               "test11" ~: "max7"  ~: (7)              ~=? (myFoldl2 max 5 [1,2,3,4,5,6,7]),
               "test12" ~: "2x+y"  ~: (43)             ~=? (myFoldl2 (\x y -> 2*x + y) 4 [1,2,3]),
               "test13" ~: "sum"   ~: 15               ~=? (myFoldr (+) 5 [1,2,3,4]),
               "test14" ~: "div"   ~: 8.0              ~=? (myFoldr (/) 2 [8,12,24,4]),
               "test15" ~: "dive"  ~: 3.0              ~=? (myFoldr (/) 3 []),
               "test16" ~: "AND"   ~: False            ~=? (myFoldr (&&) True [1>2,3>2,5==5]),
               "test17" ~: "max"   ~: 55               ~=? (myFoldr max 18 [3,6,12,4,55,11]),
               "test18" ~: "avg"   ~: 12.0             ~=? (myFoldr (\x y -> (x+y)/2) 54 [12,4,10,6]),
               -- BST unit tests [source](http://www.algolist.net/Data_structures/Binary_search_tree/Removal)
               "test19" ~: "tNoChildDel"  ~: tNoChild' ~=? (delete (-4) tNoChild),
               "test20" ~: "t1ChildDel"   ~: t1Child'  ~=? (delete 18   t1Child),
               "test21" ~: "t2ChildDel"   ~: t2Child'  ~=? (delete 12   t2Child)
             ]
