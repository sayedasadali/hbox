{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--no-termination" @-}
module Sandbox where
import Prelude     hiding (head, abs, length)
import Data.List   hiding (length)
import Data.Vector hiding (foldr)

main = putStrLn "sandbox"

{-@ type Zero    = {v:Int | v == 0} @-}
{-@ type NonZero = {v:Int | v >  0} @-}

{-@ zero :: Zero @-}
zero = 0 :: Int

{-@ measure f :: Int -> Int @-}

size :: [a] -> Int
size = foldr (\x -> (+) 1) 0
-- size [] = 0
-- size (x:xs) = 1 + size xs

{-@ die :: {v:String | false} -> a @-}
die = error

{-@ type VectorN a N = {v:Vector a | vlen v == N } @-}

{-@ threeLangs :: VectorN String 3 @-}
threeLangs = fromList [ "haskell", "javascript", "python"]

{-@ type Btwn Lo Hi = {v:Int | Lo <= v && v < Hi} @-}

{-@ (!) :: x:Vector a -> Btwn 0 (vlen x) -> a @-}

{-@ type NEVector a = {v:Vector a | 0 < vlen v} @-}

{-@ head :: NEVector a -> a @-}
head :: Vector a -> a
head vec = vec ! 0

head'' :: Vector a -> Maybe a
head'' vec
  | ok        = Just (vec ! 0)
  | otherwise = Nothing
  where
    ok        = length vec > 0

{-@ unsafeLookup :: i:Nat -> {v:Vector a | i < vlen v } -> a @-}
unsafeLookup index vec = vec ! index

-- safeLookup x i
--  | ok        = Just (x ! i)
--  | otherwise = Nothing
--   where
--     ok        = length x > 0

vectorSum :: Vector Int -> Int
vectorSum vec     = go 0 0
  where
    go acc i
      | i < sz    = go (acc + (vec ! i)) (i + 1)
      | otherwise = acc
    sz            = length vec

abs x =
  if x < 0
  then negate x
  else x

{-@ absoluteSum   :: Vector Int -> {v: Int | 0 <= v} @-}
absoluteSum       :: Vector Int -> Int
absoluteSum vec   = if 0 < sz then go 0 0 else 0
  where
    go acc i
      | i /= sz   = go (acc + abs (vec ! i)) (i + 1)
      | otherwise = acc
    sz            = length vec

loop :: Int -> Int -> a -> (Int -> a -> a) -> a
loop lo hi base f = go base lo
  where
    go acc i
      | i < hi    = go (f i acc) (i + 1)
      | otherwise = acc

dotProduct :: Vector Int -> Vector Int -> Int
{-@ dotProduct :: x:Vector Int -> VectorX Int x -> Int @-}
dotProduct x y = loop 0 sz 0 body
  where
    sz         = length x
    body i acc = acc + (x ! i) * (y ! i)

{-@ type VectorX a X = {v:Vector a | vlen v = vlen X } @-}
ex = 2

