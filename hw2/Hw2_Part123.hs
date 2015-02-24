{-# LANGUAGE TypeSynonymInstances #-}
module Hw2_Part123 where

-- extra imports for unit testing
import Control.Monad.State hiding (when)
import Data.Map hiding (foldl, foldr, delete)
import Test.HUnit hiding (State)
import Text.Parsec hiding (State, between)
import Text.Parsec.Char
import Text.Parsec.String

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



-- Part 3: An Interpreter for WHILE 
-- ================================

-- Next, you will use monads to build an evaluator for
-- a simple *WHILE* language. In this language, we will
-- represent different program variables as 

type Variable = String

-- Programs in the language are simply values of the type

data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Statement Statement   -- if (e) {s1} else {s2}
  | While Expression Statement          -- while (e) {s}
  | Sequence Statement Statement        -- s1; s2
  | Skip                                -- no-op
  deriving (Show)

-- where expressions are variables, constants or 
-- binary operators applied to sub-expressions

data Expression =
    Var Variable                        -- x
  | Val Value                           -- v 
  | Op  Bop Expression Expression
  deriving (Show)

-- and binary operators are simply two-ary functions

data Bop = 
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool 
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)

-- We will represent the *store* i.e. the machine's memory, as an associative
-- map from `Variable` to `Value` 

type Store = Map Variable Value

-- **Note:** we don't have exceptions (yet), so if a variable
-- is not found (eg because it is not initialized) simply return 
-- the value `0`. In future assignments, we will add this as a 
-- case where exceptions are thrown (the other case being type errors.)

-- We will use the standard library's `State` 
-- [monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
-- to represent the world-transformer.
-- Intuitively, `State s a` is equivalent to the world-transformer 
-- `s -> (a, s)`. See the above documentation for more details. 
-- You can ignore the bits about `StateT` for now.

-- Expression Evaluator
-- --------------------

-- First, write a function 

evalE :: Expression -> State Store Value

-- that takes as input an expression and returns a world-transformer that
-- returns a value. Yes, right now, the transformer doesnt really transform
-- the world, but we will use the monad nevertheless as later, the world may
-- change, when we add exceptions and such.

-- **Hint:** The value `get` is of type `State Store Store`. Thus, to extract 
-- the value of the "current store" in a variable `s` use `s <- get`.

evalE (Var x)      = do
          store <- get
          case Data.Map.lookup x store of
            Nothing -> return (IntVal 0)
            Just n  -> return n
evalE (Val v)      = return v
evalE (Op o e1 e2) = do
          IntVal val1 <- evalE(e1)
          IntVal val2 <- evalE(e2)
          case o of
            Plus    ->  return (IntVal  (val1 +     val2))
            Minus   ->  return (IntVal  (val1 -     val2))
            Times   ->  return (IntVal  (val1 *     val2))
            Divide  ->  return (IntVal  (val1 `div` val2))
            Gt      ->  return (BoolVal (val1 >     val2))
            Ge      ->  return (BoolVal (val1 >=    val2))
            Lt      ->  return (BoolVal (val1 <     val2))
            Le      ->  return (BoolVal (val1 <=    val2))


-- Statement Evaluator
-- -------------------

-- Next, write a function

evalS :: Statement -> State Store ()

-- that takes as input a statement and returns a world-transformer that
-- returns a unit. Here, the world-transformer should in fact update the input
-- store appropriately with the assignments executed in the course of
-- evaluating the `Statement`.

-- **Hint:** The value `put` is of type `Store -> State Store ()`. 
-- Thus, to "update" the value of the store with the new store `s'` 
-- do `put s`.

evalS w@(While e s)    = do
               v <- evalE(e)
               case v of
                IntVal  _ -> evalS Skip
                BoolVal b
                  | b     -> do
                     evalS s
                     evalS (While e s)
                  | not b -> do
                     evalS Skip
evalS Skip             = return ()
evalS (Sequence s1 s2) = do
                evalS s1
                evalS s2
evalS (Assign x e )    = do
                store <- get
                v     <- evalE(e)
                put (Data.Map.insert x v store)
evalS (If e s1 s2)     = do
                v <- evalE(e)
                case v of
                   IntVal _ -> evalS Skip
                   BoolVal b
                     | b     -> evalS s1
                     | not b -> evalS s2

-- In the `If` case, if `e` evaluates to a non-boolean value, just skip both
-- the branches. (We will convert it into a type error in the next homework.)
-- Finally, write a function 

execS   :: Statement -> Store -> Store
execS s = execState (evalS s)

-- such that `execS stmt store` returns the new `Store` that results
-- from evaluating the command `stmt` from the world `store`. 
-- **Hint:** You may want to use the library function 

-- ~~~~~{.haskell}
-- execState :: State s a -> s -> s
-- ~~~~~

-- When you are done with the above, the following function will 
-- "run" a statement starting with the `empty` store (where no 
-- variable is initialized). Running the program should print 
-- the value of all variables at the end of execution.

run :: Statement -> IO ()
run stmt = do putStrLn "Output Store:" 
              putStrLn $ show $ execS stmt empty

-- Here are a few "tests" that you can use to check your implementation.
w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

-- As you can see, it is rather tedious to write the above tests! They
-- correspond to the code in the files `test.imp` and `fact.imp`. When you are
-- done, you should get

-- ~~~~~{.haskell}
-- ghci> run w_test
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> run w_fact
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~

-- Problem 4: A Parser for WHILE 
-- =============================

-- It is rather tedious to have to specify individual programs as Haskell
-- values. For this problem, you will use parser combinators to build a parser
-- for the WHILE language from the previous problem.

-- Parsing Constants
-- -----------------

-- First, we will write parsers for the `Value` type

valueP :: Parser Value
valueP = intP <|> boolP

-- To do so, fill in the implementations of

intP :: Parser Value
intP = error "TBD" 

-- Next, define a parser that will accept a 
-- particular string `s` as a given value `x`

constP :: String -> a -> Parser a
constP s x = error "TBD"

-- and use the above to define a parser for boolean values 
-- where `"true"` and `"false"` should be parsed appropriately.

boolP :: Parser Value
boolP = error "TBD"

-- Continue to use the above to parse the binary operators

opP :: Parser Bop 
opP = error "TBD"
 

-- Parsing Expressions 
-- -------------------

-- Next, the following is a parser for variables, where each 
-- variable is one-or-more uppercase letters. 

varP :: Parser Variable
varP = many1 upper

-- Use the above to write a parser for `Expression` values

exprP :: Parser Expression
exprP = error "TBD"

-- Parsing Statements
-- ------------------

-- Next, use the expression parsers to build a statement parser

statementP :: Parser Statement
statementP = error "TBD" 

-- When you are done, we can put the parser and evaluator together 
-- in the end-to-end interpreter function

runFile s = do p <- parseFromFile statementP s
               case p of
                 Left err   -> print err
                 Right stmt -> run stmt

-- When you are done you should see the following at the ghci prompt

-- ~~~~~{.haskell}
-- ghci> runFile "test.imp"
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> runFile "fact.imp" 
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~


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
