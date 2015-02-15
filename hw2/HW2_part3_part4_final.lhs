Part 3: An Interpreter for WHILE 
================================

Next, you will use monads to build an evaluator for
a simple *WHILE* language. In this language, we will
represent different program variables as 

> type Variable = String

Programs in the language are simply values of the type

> data Statement =
>     Assign Variable Expression          -- x = e
>   | If Expression Statement Statement   -- if (e) {s1} else {s2}
>   | While Expression Statement          -- while (e) {s}
>   | Sequence Statement Statement        -- s1; s2
>   | Skip                                -- no-op
>   deriving (Show)

where expressions are variables, constants or 
binary operators applied to sub-expressions

> data Expression =
>     Var Variable                        -- x
>   | Val Value                           -- v 
>   | Op  Bop Expression Expression
>   deriving (Show)

and binary operators are simply two-ary functions

> data Bop = 
>     Plus     -- +  :: Int  -> Int  -> Int
>   | Minus    -- -  :: Int  -> Int  -> Int
>   | Times    -- *  :: Int  -> Int  -> Int
>   | Divide   -- /  :: Int  -> Int  -> Int
>   | Gt       -- >  :: Int -> Int -> Bool 
>   | Ge       -- >= :: Int -> Int -> Bool
>   | Lt       -- <  :: Int -> Int -> Bool
>   | Le       -- <= :: Int -> Int -> Bool
>   deriving (Show)

> data Value =
>     IntVal Int
>   | BoolVal Bool
>   deriving (Show)

We will represent the *store* i.e. the machine's memory, as an associative
map from `Variable` to `Value` 

> type Store = Map Variable Value

**Note:** we don't have exceptions (yet), so if a variable
is not found (eg because it is not initialized) simply return 
the value `0`. In future assignments, we will add this as a 
case where exceptions are thrown (the other case being type errors.)

We will use the standard library's `State` 
[monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
to represent the world-transformer.
Intuitively, `State s a` is equivalent to the world-transformer 
`s -> (a, s)`. See the above documentation for more details. 
You can ignore the bits about `StateT` for now.

Expression Evaluator
--------------------

First, write a function 

> evalE :: Expression -> State Store Value

that takes as input an expression and returns a world-transformer that
returns a value. Yes, right now, the transformer doesnt really transform
the world, but we will use the monad nevertheless as later, the world may
change, when we add exceptions and such.

**Hint:** The value `get` is of type `State Store Store`. Thus, to extract 
the value of the "current store" in a variable `s` use `s <- get`.

> -- use the lookup to define cases
> evalE (Var x)      = 	do
>			store 	<- get -- as suggested above
>			case Data.Map.lookup x store of 
>				Nothing -> return $ IntVal 0
>				Just n 	-> return n
> -- just return v in this case
> evalE (Val v)      = return v 
>
> evalE (Op o e1 e2) = do 
>
>	IntVal val1 <- evalE(e1)
>	IntVal val2 <- evalE(e2)
>	case o of 		--need to define all possible operators here
>			Plus 	-> return $ IntVal $ val1 + val2   		--1
>			Minus 	-> return $ IntVal $ val1 - val2		--2
>			Times 	-> return $ IntVal $ val1 * val2		--3
>			Divide 	-> return $ IntVal $ val1 `div` val2		--4
>                       Gt     	-> return $ BoolVal $ val1 > val2		--5
>                       Ge      -> return $ BoolVal $ val1 >= val2		--6
>                       Lt      -> return $ BoolVal $ val1 < val2		--7
>                       Le      -> return $ BoolVal $ val1 <= val2		--8


Statement Evaluator
-------------------

Next, write a function

> evalS :: Statement -> State Store ()

that takes as input a statement and returns a world-transformer that
returns a unit. Here, the world-transformer should in fact update the input
store appropriately with the assignments executed in the course of
evaluating the `Statement`.

**Hint:** The value `put` is of type `Store -> State Store ()`. 
Thus, to "update" the value of the store with the new store `s'` 
do `put s`.

> evalS w@(While e s)    = do	
>				v <- evalE(e)
>				case v of  -- need to define for both IntVal and BoolVal
>					IntVal _ -> evalS Skip
>					BoolVal v 	| v -> do
>								evalS s
>								evalS $ While e s
>							| not v -> do 
>								evalS Skip
>
>

> evalS Skip             = return ()
>
> evalS (Sequence s1 s2) = do 
> 				evalS s1
>				evalS s2

> evalS (Assign x e )    = do
>				store 	<- get
>				v 	<- evalE(e)
>				put 	$ Data.Map.insert x v store

> evalS (If e s1 s2)     = do
>				v <- evalE(e)
>				case v of 
>					IntVal _  			-> evalS Skip
>					BoolVal v 	| v 		->evalS s1
>							| not v 	->evalS s2

In the `If` case, if `e` evaluates to a non-boolean value, just skip both
the branches. (We will convert it into a type error in the next homework.)
Finally, write a function 

> execS :: Statement -> Store -> Store
> execS s = execState $ evalS s

such that `execS stmt store` returns the new `Store` that results
from evaluating the command `stmt` from the world `store`. 
**Hint:** You may want to use the library function 

~~~~~{.haskell}
execState :: State s a -> s -> s
~~~~~

When you are done with the above, the following function will 
"run" a statement starting with the `empty` store (where no 
variable is initialized). Running the program should print 
the value of all variables at the end of execution.

> run :: Statement -> IO ()
> run stmt = do putStrLn "Output Store:" 
>               putStrLn $ show $ execS stmt empty

Here are a few "tests" that you can use to check your implementation.

> w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

> w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

As you can see, it is rather tedious to write the above tests! They
correspond to the code in the files `test.imp` and `fact.imp`. When you are
done, you should get

~~~~~{.haskell}
ghci> run w_test
Output Store:
fromList [("X",IntVal 0),("Y",IntVal 10)]

ghci> run w_fact
Output Store:
fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
~~~~~

Problem 4: A Parser for WHILE 
=============================

It is rather tedious to have to specify individual programs as Haskell
values. For this problem, you will use parser combinators to build a parser
for the WHILE language from the previous problem.

Parsing Constants
-----------------

First, we will write parsers for the `Value` type

> valueP :: Parser Value
> valueP = intP <|> boolP

To do so, fill in the implementations of

> intP :: Parser Value
> intP = do skipMany space
>	    numStr <- many1 digit
>	    return (IntVal (read numStr::Int)) 

Next, define a parser that will accept a 
particular string `s` as a given value `x`

> constP :: String -> a -> Parser a
> constP s x = do skipMany space
> 		  string s
> 		  return x

and use the above to define a parser for boolean values 
where `"true"` and `"false"` should be parsed appropriately.

> boolP :: Parser Value
> boolP = try (constP "true" (BoolVal True)) <|>
>	  try (constP "false" (BoolVal False))

Continue to use the above to parse the binary operators

> mulOp :: Parser Bop
> mulOp = try (constP "*" Times ) <|>
>         try (constP "/" Divide)
> addOp :: Parser Bop
> addOp = try (constP "+" Plus ) <|>
>         try (constP "-" Minus )
> relOp :: Parser Bop
> relOp = try (constP ">=" Ge ) <|>
>         try (constP ">"  Gt ) <|>
>         try (constP "<=" Le ) <|>
>         try (constP "<"  Lt )
> opP :: Parser Bop 
> opP = mulOp <|> addOp <|> relOp

Parsing Expressions 
-------------------
Next, the following is a parser for variables, where each 
variable is one-or-more uppercase letters. 

> varP :: Parser Variable
> varP = do skipMany space
>	    many1 upper

Use the above to write a parser for `Expression` values

> parenP :: Parser a -> Parser a
> parenP p = do constP "(" ()
>		x <- p
>		constP ")" ()
>		return x

Need to convert relOp addOp and mulOp to expression fmap f z = do x <- z return f x

> exprP :: Parser Expression
> exprP = exprP2 `chainl1` (Op `fmap` relOp)
>
> exprP2 :: Parser Expression
> exprP2 = exprP3 `chainl1` (Op `fmap` addOp)
>
> exprP3 :: Parser Expression
> exprP3 = factorE `chainl1` (Op `fmap` mulOp)
>
> factorE :: Parser Expression
> factorE = try (Var `fmap` varP) <|> try (Val `fmap` valueP) <|> try (parenP exprP)

Parsing Statements
------------------
Next, use the expression parsers to build a statement parser

> assignStt :: Parser Statement
> assignStt = do varParser <- varP
>		 constP ":=" ()
>		 exp <- exprP
>		 return $ Assign varParser exp
>
> ifStt :: Parser Statement
> ifStt = do constP "if" ()
>	     exp <- exprP
>	     constP "then" ()
>	     stt1 <- statementP
>	     constP "else" ()
>	     stt2 <- statementP
>	     constP "endif" ()
>	     return $ If exp stt1 stt2
>
> whileStt :: Parser Statement
> whileStt = do constP "while" ()
>		exp <- exprP
>		constP "do" ()
>		stt <- statementP
>		constP "endwhile" ()
>		return $ While exp stt
>
> semiColon :: Parser (Statement -> Statement -> Statement)
> semiColon = try (constP ";" ()) >> return Sequence
>
> seqStt :: Parser Statement
> seqStt = seqStP `chainl1` semiColon
>
> skipStt :: Parser Statement
> skipStt = constP "skip" Skip
>
> seqStP :: Parser Statement
> seqStP = try (ifStt) <|> try (whileStt) <|> try (assignStt) <|> skipStt
> statementP :: Parser Statement
> statementP = do s <- seqStt
>		  return s

When you are done, we can put the parser and evaluator together 
in the end-to-end interpreter function

> runFile s = do p <- parseFromFile statementP s
>                case p of
>                  Left err   -> print err
>                  Right stmt -> run stmt

When you are done you should see the following at the ghci prompt
~~~~~{.haskell}
ghci> runFile "test.imp"
Output Store:
fromList [("X",IntVal 0),("Y",IntVal 10)]
ghci> runFile "fact.imp" 
Output Store:
fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
~~~~~

