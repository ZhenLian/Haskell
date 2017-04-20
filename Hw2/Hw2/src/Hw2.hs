-- ---
-- title: Homework #2, Due Friday 2/12/16
-- ---

{-# LANGUAGE TypeSynonymInstances #-}
module Hw2 where

import Control.Applicative hiding (empty, (<|>))
import Data.Map hiding (delete)
import Control.Monad.State hiding (when)
import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String

-- Problem 0: All About You
-- ========================

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Zhen Lian"
myEmail = "z3lian@eng.ucsd.edu"
mySID   = "A53215552(PID)"


-- Problem 1: All About `foldl`
-- ============================

-- Define the following functions by filling in the "error" portion:

-- 1. Describe `foldl` and give an implementation:

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f b []    = b 
myFoldl f b (x:xs) = myFoldl f (f b x) xs


-- 2. Using the standard `foldl` (not `myFoldl`), define the list reverse function:

myReverse :: [a] -> [a]
myReverse xs = Prelude.foldl (\acc x -> x:acc) [] xs

-- 3. Define `foldr` in terms of `foldl`:

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b xs = Prelude.foldl (flip f) b (myReverse xs)

-- 4. Define `foldl` in terms of the standard `foldr` (not `myFoldr`):

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f b xs = Prelude.foldr (flip f) b (myReverse xs)

-- 5. Try applying `foldl` to a gigantic list. Why is it so slow?
--    Try using `foldl'` (from [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#3))
--    instead; can you explain why it's faster?

--Because Haskell uses lazy evaluation, which means that:
--1.foldl is non-strict function. It doesn't evaluate expressions until it's required to do so. And there will be many computation overhead during the calculation step 
--2.using `foldl` may result in stack overflow.
--I think maybe Data.List.foldl uses the implementation of foldr to realize foldl, so it is faster 

-- Part 2: Binary Search Trees
-- ===========================

-- Recall the following type of binary search trees:

data BST k v = Emp
             | Bind k v (BST k v) (BST k v)
             deriving (Show)

-- Define a `delete` function for BSTs of this type:
--let t = Bind 4 'a'(Bind 2 'b'(Bind 1 'd'(Emp)(Emp))(Bind 3 'e' (Emp)(Emp)))(Bind 7 'c' (Emp)(Emp))
delete :: (Ord k) => k -> BST k v -> BST k v
delete _ Emp  =  Emp
delete k (Bind kcur v l r)
                      | k<kcur    =  Bind kcur v (Hw2.delete k l) r
                      | k>kcur    =  Bind kcur v l (Hw2.delete k r)
                      | otherwise =  case (l,r) of
                                             (_,Emp) -> l
                                             (Emp,_) -> r
                                             (_,_)   -> let (knew,vnew) = findLMax l
                                                        in Bind knew vnew (Hw2.delete knew l) r


findLMax :: (BST k v) -> (k,v)
findLMax (Bind k v l Emp) = (k,v)
findLMax (Bind k v _ r)   = findLMax r

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
    Plus     -- (+)  :: Int  -> Int  -> Int
  | Minus    -- (-)  :: Int  -> Int  -> Int
  | Times    -- (*)  :: Int  -> Int  -> Int
  | Divide   -- (/)  :: Int  -> Int  -> Int
  | Gt       -- (>)  :: Int -> Int -> Bool
  | Ge       -- (>=) :: Int -> Int -> Bool
  | Lt       -- (<)  :: Int -> Int -> Bool
  | Le       -- (<=) :: Int -> Int -> Bool
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

evalOp :: Bop -> Value -> Value -> Value
evalOp Plus   (IntVal i) (IntVal j) = IntVal (i + j)
evalOp Minus  (IntVal i) (IntVal j) = IntVal (i - j)
evalOp Times  (IntVal i) (IntVal j) = IntVal (i * j)
evalOp Divide (IntVal i) (IntVal j) = IntVal (i `safediv` j)where
  safediv n m = if m == 0 then 0 else n `div` m
evalOp Gt     (IntVal i) (IntVal j) = BoolVal (i > j)
evalOp Ge     (IntVal i) (IntVal j) = BoolVal (i >= j)
evalOp Lt     (IntVal i) (IntVal j) = BoolVal (i < j)
evalOp Le     (IntVal i) (IntVal j) = BoolVal (i <= j) 

-- >

evalE (Var x)      = do
                       store <- get
                       return (findWithDefault (IntVal 0) x store) 
                   
evalE (Val v)      = return v
evalE (Op o e1 e2) = do 
                       (IntVal v1) <- evalE e1
                       (IntVal v2) <- evalE e2
                       return (evalOp o (IntVal v1) (IntVal v2))
-- Statement Evaluator
-- -------------------

-- Next, write a function

evalS :: Statement -> State Store () 

-- what is the () used for? Is it aimed to discard the result and concentrate on the state?
-- that takes as input a statement and returns a world-transformer that
-- returns a unit. Here, the world-transformer should in fact update the input
-- store appropriately with the assignments executed in the course of
-- evaluating the `Statement`.

-- **Hint:** The value `put` is of type `Store -> State Store ()`.
-- Thus, to "update" the value of the store with the new store `s'`
-- do `put s'`.

evalS (Assign x e )    = do
                           store <- get
                           val <- evalE e
                           put (insert x val store)
evalS w@(While e s)    = do
                           val <- evalE e
                           case val of
                             BoolVal True  ->  do
                                                evalS s
                                                evalS w
                             BoolVal False ->  return ()
                             IntVal   _    -> evalS Skip
evalS Skip             = return ()
evalS (Sequence s1 s2) = do
                          evalS s1
                          evalS s2
evalS (If e s1 s2)     = do
                          val <- evalE e
                          case val of 
                             BoolVal True  -> evalS s1
                             BoolVal False -> evalS s2
                             IntVal   _    -> evalS Skip


-- In the `If` case, if `e` evaluates to a non-boolean value, just skip both
-- the branches. (We will convert it into a type error in the next homework.)
-- Finally, write a function

execS :: Statement -> Store -> Store
execS stmt store = execState (evalS stmt) store

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
intP = do
         x <- many1 digit
         return (IntVal (read x))

-- Next, define a parser that will accept a
-- particular string `s` as a given value `x`

constP :: String -> a -> Parser a
constP s x = do
               tmp <- string s
               return x

-- and use the above to define a parser for boolean values
-- where `"true"` and `"false"` should be parsed appropriately.

boolP :: Parser Value
boolP = do 
  (constP "true"  (BoolVal True))<|> (constP "false" (BoolVal False))

-- Continue to use the above to parse the binary operators

opP :: Parser Bop
opP = do
       try $ constP ">=" Ge
       <|> do
           try $ constP "<=" Le
           <|> constP "+" Plus
           <|> constP "-" Minus
           <|> constP "*" Times
           <|> constP "/" Divide
           <|> constP "<" Lt
           <|> constP ">" Gt
           


-- Parsing Expressions
-- -------------------

-- Next, the following is a parser for variables, where each
-- variable is one-or-more uppercase letters.

varP :: Parser Variable
varP = many1 upper

-- Use the above to write a parser for `Expression` values


exprVariable :: Parser Expression
exprVariable = do
        variable <- varP
        return (Var variable)

exprValue :: Parser Expression
exprValue = do
        value <- valueP
        return (Val value)

exprInParenthsis :: Parser Expression
exprInParenthsis = do
             char '('
             skipMany space
             expressionInParen <- exprP
             skipMany space
             char ')'
             return expressionInParen

exprBop :: Parser Expression
exprBop = do
        skipMany space
        leftpart <- exprVariable <|> exprValue <|> exprInParenthsis
        skipMany space
        op <- opP
        skipMany space
        rightpart <- exprVariable <|> exprValue <|> exprInParenthsis
        skipMany space
        return (Op op leftpart rightpart)

exprP :: Parser Expression
exprP = do
        try $ exprBop
        <|> do
            try $ exprInParenthsis
            <|> exprVariable
            <|> exprValue



-- Parsing Statements
-- ------------------

-- Next, use the expression parsers to build a statement parser

assignP :: Parser Statement
assignP = do
          skipMany space
          var <- varP
          skipMany space
          string ":="
          skipMany space
          expr <- exprP
          return (Assign var expr)

ifP :: Parser Statement
ifP = do
      skipMany space
      string "if"
      skipMany space
      condition <- exprP
      skipMany space
      string "then"
      skipMany space
      s1 <- statementP
      skipMany space
      string "else"
      skipMany space
      s2 <- statementP
      skipMany space
      string "endif"
      return (If condition s1 s2)

whileP :: Parser Statement
whileP = do
       skipMany space
       string "while"
       skipMany space
       condition <- exprP
       skipMany space
       string "do"
       skipMany space
       s <- statementP
       skipMany space
       string "endwhile"
       return (While condition s)

seqP :: Parser Statement
seqP = do
     skipMany space
     s1 <- assignP <|> ifP <|> whileP <|> skipP
     skipMany space
     char ';'
     skipMany space
     s2 <- statementP
     return (Sequence s1 s2)

skipP :: Parser Statement
skipP = do
      string "skip"
      return (Skip)

statementP :: Parser Statement
statementP = do
             try seqP
                 <|> whileP
                 <|> skipP
                 <|> assignP
                 <|> ifP
                 

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





