-- ---
-- title: Homework #3, Due Monday, Feb 29, 2016 (23:59:59 PST)
-- ---

-- Preliminaries
-- =============

-- To complete this homework,

-- 1. download [Hw3.tar.gz](../static/Hw2.tgz),
-- 2. unzip it by `tar -zxvf Hw3.tgz`
-- 3. `cd Hw3` ,
-- 4. Fill in each `error "TODO"` in `src/Hw3.hs`
-- 5. Submit by mailing the completed `Hw3.hs` to `cse230@goto.ucsd.edu` with the
--    subject "HW3".

-- You will receive a confirmation email after submitting.

-- Your code *must* typecheck against the given type signatures.
-- Feel free to add your own tests to this file to exercise the
-- functions you write. As before, you can compile the code by
-- doing `stack build` and load in `ghci` by doing `stack ghci`.

-- **Learn to read the [documentation](http://hackage.haskell.org)**

{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveGeneric             #-}

module Hw3 where

import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import GHC.Generics
import Test.QuickCheck hiding ((===))
import Control.Monad (forM, forM_)
import Data.List (transpose, intercalate)


quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n}

-- Problem 0: All About You
-- ========================

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Zhen Lian"
myEmail = "z3lian@eng.ucsd.edu"
mySID   = "A53215552(PID)"



-- Problem 1: An Interpreter for WHILE++
-- =====================================

-- Previously, you wrote a simple interpreter for *WHILE*.
-- For this problem, you will use monad transformers to build
-- an evaluator for *WHILE++* which, adds exceptions and I/O
-- to the original language.

-- As before, we have variables, and expressions.

type Variable = String
type Store    = Map.Map Variable Value
-- >
data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show, Generic)
-- >
instance Error Value
-- >
data Expression =
    Var Variable
  | Val Value
  | Op  Bop Expression Expression
  deriving (Show)
-- >
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  deriving (Show)

-- Programs in the language are simply values of the type

data Statement =
    Assign Variable Expression
  | If Expression Statement Statement
  | While Expression Statement
  | Sequence Statement Statement
  | Skip
  | Print String Expression
  | Throw Expression
  | Try Statement Variable Statement
  deriving (Show)

-- Write a function

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

-- findWithDefault :: Ord k => a -> k -> Map k a -> a
-- The expression (findWithDefault def k map) returns the value at key k or returns default value def when the key is not in the map.
evalE :: (MonadState Store m) => Expression -> m Value
evalE (Var x)      = do
                       store <- get
                       return (Map.findWithDefault (IntVal 0) x store) 
                   
evalE (Val v)      = return v
evalE (Op o e1 e2) = do 
                       (IntVal v1) <- evalE e1
                       (IntVal v2) <- evalE e2
                       return (evalOp o (IntVal v1) (IntVal v2))
-- The only new constructs are the `Print`, `Throw` and the `Try` statements.

-- - `Print s e` should print out (eg to stdout) log the string corresponding
--   to the string `s` followed by whatever `e` evaluates to, followed by a
--   newline --- for example, `Print "Three: " (IntVal 3)' should display
--   "Three: IntVal 3\n",

-- - `Throw e` evaluates the expression `e` and throws it as an exception, and

-- - `Try s x h` executes the statement `s` and if in the course of
--   execution, an exception is thrown, then the exception comes shooting
--   up and is assigned to the variable `x` after which the *handler*
--   statement `h` is executed.

-- We will use the `State` [monad][2] to represent the world-transformer.
-- Intuitively, `State s a` is equivalent to the world-transformer
-- `s -> (a, s)`. See the above documentation for more details.
-- You can ignore the bits about `StateT` for now.

-- insert :: Ord k => k -> a -> Map k a -> Map k a
-- Insert a new key and value in the map. If the key is already present in the map, the associated value is replaced with the supplied value. insert is equivalent to insertWith const.
-- catchError :: MonadError e m => m a -> (e -> m a) -> m a
evalS :: (MonadState Store m, MonadError Value m, MonadWriter String m) => Statement -> m ()
evalS (Assign x e )    = do
                           store <- get
                           val <- evalE e
                           put (Map.insert x val store)
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

evalS (Print s e)      = do
                          val <- evalE e
                          tell(s ++ show(val) ++ "\n")

evalS (Throw e)        = do 
                          val <- evalE e
                          throwError val
-- catchError :: MonadError e m => m a -> (e -> m a) -> m a
-- take in a monad and an error handler. If the function has error inside the monad, use error handler
-- to take that error and output a new monad. Else output the original monad
-- same as the trycatch function in the text book
evalS (Try s x h)      = do    
                          catchError (evalS s) errorHandler
                          where errorHandler err = do
                                        store <- get
                                        put (Map.insert x err store)
                                        evalS h


--Helpful type information(from another website)
--  get :: MonadState s m => m s
--  put :: MonadState s m => s -> m ()
--  tell :: MonadWriter w m => w -> m ()
--  throwError :: MonadError e m => e -> m a
--  catchError :: MonadError e m => m a -> (e -> m a) -> m a
--  runState :: State s a -> s -> (a, s)
--  runStateT :: StateT s m a -> s -> m (a, s)
--  runWriter :: Writer w a -> (a, w)
--  runWriterT :: WriterT w m a -> m (a, w)
--  runErrorT :: ErrorT e m a -> m (Either e a)

-- Next, we will implement a *concrete instance* of a monad `m` that
-- satisfies the above conditions, by filling in a suitable definition:

type Eval a = ErrorT Value (WriterT String (State Store)) a

-- Now, we implement a function to *run* the action from a given store:

runEval :: Eval a -> Store -> ((Either Value a, String), Store)
runEval act sto = runState (runWriterT(runErrorT act)) sto

-- When you are done, you will get an implementation:

execute :: Store -> Statement -> (Store, Maybe Value, String)
execute sto stmt   = (sto', leftMaybe v, l)
  where
    ((v, l), sto') = runEval (evalS stmt) sto

leftMaybe :: Either a b -> Maybe a
leftMaybe (Left v)  = Just v
leftMaybe (Right _) = Nothing


-- such that `execute st s` returns a triple `(st', exn, log)` where

-- - `st'` is the output state,
-- - `exn` is possibly an exception (if the program terminates with an uncaught exception),
-- - `log` is the log of messages generated by the `Print` statements.

-- Requirements
-- ------------

-- In the case of exceptional termination, the `st'` should be the state *at
-- the point where the last exception was thrown, and `log` should include all
-- the messages *upto* that point -- make sure you stack (*order*) your transformers
-- appropriately!

-- - Reading an undefined variable should raise an exception carrying the value `IntVal 0`.

-- - Division by zero should raise an exception carrying the value `IntVal 1`.

-- - A run-time type error (addition of an integer to a boolean, comparison of
--   two values of different types) should raise an exception carrying the value
--   `IntVal 2`.

-- Example 1
-- ---------

-- If `st` is the empty state (all variables undefined) and `s` is the program

-- ~~~~~{.haskell}
-- X := 0 ;
-- Y := 1 ;
-- print "hello world: " X;
-- if X < Y then
--   throw (X+Y)
-- else
--   skip
-- endif;
-- Z := 3
-- ~~~~~

-- then `execute st s` should return the triple
-- execute Map.empty testprog1
-- ~~~~~{.haskell}
-- (fromList [("X", IntVal 0), ("Y",  IntVal 1)], Just (IntVal 1), "hello world: IntVal 0\n")
-- ~~~~~

-- The program is provided as a Haskell value below:

mksequence = foldr Sequence Skip

testprog1 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Assign "Y" $ Val $ IntVal 1,
                        Print "hello world: " $ Var "X",
                        If (Op Lt (Var "X") (Var "Y")) (Throw (Op Plus (Var "X") (Var "Y")))
                                                       Skip,
                        Assign "Z" $ Val $ IntVal 3]

-- Example 2
-- ---------

-- If `st` is the empty state (all variables undefined) and `s` is the program

-- ~~~~~{.haskell}
-- X := 0 ;
-- Y := 1 ;
-- try
--   if X < Y then
--     A := 100;
--     throw (X+Y);
--     B := 200
--   else
--     skip
--   endif;
-- catch E with
--   Z := E + A
-- endwith
-- ~~~~~

-- then `execute st s` should return the triple
-- execute Map.empty testprog2
-- ~~~~~{.haskell}
-- ( fromList [("A", IntVal 100), ("E", IntVal 1)
--            ,("X", IntVal 0), ("Y", IntVal 1)
--  	   ,("Z", IntVal 101)]
-- , Nothing
-- , "")
-- ~~~~~

-- Again, the program as a Haskell value:

testprog2 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Assign "Y" $ Val $ IntVal 1,
                        Try (If (Op Lt (Var "X") (Var "Y"))
                                (mksequence [Assign "A" $ Val $ IntVal 100,
                                             Throw (Op Plus (Var "X") (Var "Y")),
                                             Assign "B" $ Val $ IntVal 200])
                                Skip)
                            "E"
                            (Assign "Z" $ Op Plus (Var "E") (Var "A"))]


-- Problem 2: Binary Search Trees Revisited
-- ========================================

-- Recall the old type of binary search trees from
-- [HW2](/homeworks/Hw2.html).

data BST k v = Emp
             | Bind k v (BST k v) (BST k v)
             deriving (Show)
-- >flat the tree?
toBinds ::  BST t t1 -> [(t, t1)]
toBinds Emp            = []
toBinds (Bind k v l r) = toBinds l ++ [(k,v)] ++ toBinds r

-- The following function tests whether a tree satisfies the
-- binary-search-order invariant.
-- all :: (a -> Bool) -> [a] -> Bool
-- Applied to a predicate and a list, all determines if all elements of the list satisfy the predicate.
isBSO ::  Ord a => BST a b -> Bool
isBSO Emp            = True
isBSO (Bind k v l r) = all (< k) lks && all (k <) rks && isBSO l && isBSO r
  where lks = map fst $ toBinds l
        rks = map fst $ toBinds r

-- Finally, to test your implementation, we will define a
-- type of operations over trees

data BSTop k v = BSTadd k v | BSTdel k
                 deriving (Eq, Show)

-- and a function that constructs a tree from a sequence of operations
-- foldr :: (a -> b -> b) -> b -> [a] -> b
ofBSTops ::  Ord k => [BSTop k v] -> BST k v
ofBSTops    = foldr doOp Emp
  where doOp (BSTadd k v) = bstInsert k v
        doOp (BSTdel k)   = bstDelete k

-- and that constructs a reference `Map` from a sequence of operations

mapOfBSTops ::  Ord k => [BSTop k a] -> Map.Map k a
mapOfBSTops = foldr doOp Map.empty
  where doOp (BSTadd k v) = Map.insert k v
        doOp (BSTdel k)   = Map.delete k

-- and functions that generate an arbitrary BST operations

keys :: [Int]
keys = [0..10]
-- >
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
-- Promote a function to a monad, scanning the monadic arguments from left to right.
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- Promote a function to a monad.
-- elements :: [a] -> Gen a
-- Generates one of the given values. The input list must be non-empty.
-- frequency :: [(Int, Gen a)] -> Gen a
-- Chooses one of the given generators, with a weighted random distribution. The input list must be non-empty.
genBSTadd, genBSTdel, genBSTop ::  Gen (BSTop Int Char)
genBSTadd = liftM2 BSTadd (elements keys) (elements ['a'..'z'])
genBSTdel = liftM BSTdel (elements keys)
genBSTop  = frequency [(5, genBSTadd), (1, genBSTdel)]

-- (a) Insertion
-- -------------
--AVL Tree  

--The right child rotation 
bstRotR :: BST k v -> BST k v
bstRotR Emp                               = Emp   -- error "cannot rotate left children"
bstRotR (Bind k v l (Bind rk rv rl rr) )  = Bind rk rv (Bind k v l rl) rr 


--The left child's rotation 

bstRotL :: BST k v -> BST k v 
bstRotL Emp                               = Emp -- error "cannot rotate right"
bstRotL (Bind k v  (Bind lk lv ll lr) r ) = Bind  lk lv ll (Bind k v lr r)  

--calculate height & diff value between left and right children 

bsthd :: BST k v -> (Int, Int)
bsthd (Bind k v l r) = (  1 + ( max h_l h_r), h_l - h_r  )
  where h_l = height l
        h_r = height r
bsthd Emp = (0, 0)
{--
For the Balance of AVL tree via rotation to deal with 
The situation: 
The rotation from bottom (depth 2 for father) up
(1) LL : rotL
(2) LR : rotL and rotR
(3) RR : rotR
(4) RL : rotR and rotL
--}
bstRotBal ::  Int ->  Int -> Int -> Int -> Int -> BST k v -> Either ( BST k v) (Int, Int, BST k v) 
-- diff_l is the difference of the left sub tree and diff_r is that of right sub tree
-- Right means no need to be balanced
bstRotBal diff_fa  height_l  diff_l   height_r diff_r (Bind k v l r)   
      | diff_fa > 1  &&  diff_l >= 0 =  Left $ bstRotL $ ( Bind k v l r)  
      | diff_fa > 1  &&  diff_l <  0 =  Left $ bstRotL $ ( Bind k v  (bstRotR l) r)
      | diff_fa < -1 &&  diff_r <= 0 =  Left $ bstRotR $ ( Bind k v  l r )
      | diff_fa < -1 &&  diff_r >  0 =  Left $ bstRotR $ ( Bind k v l (bstRotL r)  ) 
      | otherwise = Right ( hei , diff_fa  , (Bind k v  l r)  )
      where hei = 1 + max height_l height_r


-- Real Insertion via height and  height difference 

balInsert :: (Ord k) => k -> v -> BST k v -> Either (BST k v ) (Int, Int, BST k v)
balInsert k' v' Emp = Right (1, 0 ,( Bind k' v' Emp Emp))
balInsert k' v' (Bind k v l r) 
  | k' == k   = Left  (Bind k' v' l r)
  | k' < k    = case balInsert k' v' l of        -- check the condition of left sub tree
                  Right (height_l, diff_l , l' ) -- find the smallest balanced tree and balance current node
                      -> bstRotBal (height_l- height_r) height_l diff_l height_r diff_r (Bind k v l' r)
                           where (height_r, diff_r) = bsthd r 
                  Left l'                        -- keep searching  until finding the smallest balanced tree
                      -> Left $ (Bind k v l' r) 
  | otherwise = case balInsert k' v' r of 
                  Right (height_r, diff_r , r' ) 
                      -> bstRotBal (height_l- height_r) height_l diff_l height_r diff_r (Bind k v l r')
                           where (height_l, diff_l) = bsthd l 
                  Left r'   
                      -> Left $ (Bind k v l r') 


--Write an insertion function 

-- either :: (a -> c) -> (b -> c) -> Either a b -> c
-- Case analysis for the Either type. If the value is Left a, apply the first function to a; if it is Right b, apply the second function to b.
-- id :: a -> a
-- Identity function.

bstInsert :: (Ord k) => k -> v -> BST k v -> BST k v
bstInsert k v = either id ( \(k', v', bstI)-> bstI ) . balInsert k v 



-- original unbalanced tree inserting function
-- Write an insertion function
{--
bstInsert :: (Ord k) => k -> v -> BST k v -> BST k v
bstInsert key value Emp               = (Bind key value Emp Emp)
bstInsert key value (Bind k v l r)    = case (key<k) of
                                                True  -> (Bind k v (bstInsert key value l) r)
                                                False -> case (key>k) of
                                                                 True  -> (Bind k v l (bstInsert key value r))
                                                                 False ->  Bind key value l r
--}
-- such that `bstInsert k v t` inserts a key `k` with value
-- `v` into the tree `t`. If `k` already exists in the input
-- tree, then its value should be *replaced* with `v`. When you
-- are done, your code should satisfy the following QC properties.

prop_insert_bso :: Property
prop_insert_bso = forAll (listOf genBSTadd) $ \ops ->
                    isBSO (ofBSTops ops)
-- >
prop_insert_map = forAll (listOf genBSTadd) $ \ops ->
                    toBinds (ofBSTops ops) == Map.toAscList (mapOfBSTops ops)

-- (b) Deletion
-- ------------

-- Write a deletion function for BSTs of this type:
findLMax :: (BST k v) -> (k,v)
findLMax (Bind k v l Emp) = (k,v)
findLMax (Bind k v _ r)   = findLMax r 

balbst :: BST k v -> BST k v
balbst (Bind k v l r)  
    | dlr (Bind k v l r) > 1  && dlr l >= 0 = bstRotL $ (Bind k v l r) 
    | dlr (Bind k v l r) > 1  && dlr l < 0  = bstRotL $ (Bind k v (bstRotR l) r) 
    | dlr (Bind k v l r) < -1 && dlr r <= 0 = bstRotR $ (Bind k v l r) 
    | dlr (Bind k v l r) < -1 && dlr r > 0  = bstRotR $ (Bind k v l (bstRotL r)) 
    | otherwise  = Bind k v l r
    where dlr Emp            = 0 
          dlr (Bind k v l r) = height l - height r
   

bstDelete :: (Ord k) => k -> BST k v -> BST k v
bstDelete _ Emp = Emp
bstDelete k (Bind kk v l r) 
   | k > kk = balbst $ (Bind kk v l (bstDelete k r))
   | k < kk = balbst $ (Bind kk v (bstDelete k l) r)
   | otherwise = case ( l, r) of 
          (_  , Emp) -> l
          (Emp, _  ) -> r
          (_  , _  ) -> let ( kt , vt) = findLMax l
                            in balbst $ (Bind kt vt (bstDelete kt l ) r)

{-- original code
bstDelete :: (Ord k) => k -> BST k v -> BST k v 
bstDelete _ Emp  =  Emp
bstDelete kdel (Bind kcur v l r)
                      | kdel<kcur    =  Bind kcur v (bstDelete kdel l) r
                      | kdel>kcur    =  Bind kcur v l (bstDelete kdel r)
                      | otherwise =  case (l,r) of
                                             (_,Emp) -> l
                                             (Emp,_) -> r
                                             (_,_)   -> let (knew,vnew) = findLMax l
                                                        in Bind knew vnew (bstDelete knew l) r

--}
-- such that `bstDelete k t` removes the key `k` from the tree `t`.
-- If `k` is absent from the input tree, then the tree is returned
-- unchanged as the output. When you are done, your code should
-- satisfy the following QC properties.

prop_delete_bso :: Property
prop_delete_bso = forAll (listOf genBSTop) $ \ops ->
                    isBSO (ofBSTops ops)
-- >
prop_delete_map = forAll (listOf genBSTop) $ \ops ->
                    toBinds (ofBSTops ops) == Map.toAscList (mapOfBSTops ops)


-- (c) Balanced Trees
-- ------------------

-- The following function determines the `height` of a BST

height (Bind _ _ l r) = 1 + max (height l) (height r)
height Emp            = 0

-- We say that a tree is *balanced* if

isBal (Bind _ _ l r) = isBal l && isBal r && abs (height l - height r) <= 2
isBal Emp            = True

-- Write a balanced tree generator
-- code in let is the function only to check whether the tree is balanced if inserted left(or right)
-- if the condition(where insert left)it is balanced, then do it; If condition(where insert left) is no
-- more balanced, then do inserting right; If both of them are not balanced, which means the tree cannot be
-- balanced in this step, so I try another chance
-- notice that every recursion I get two random number klt and kgt. The insertion position of new node
-- is determined by the property of BST, this function just check whether the tree is balanced after insertion!

genBal' :: Int -> (BST Int Char) -> Gen (BST Int Char)
genBal' 0 t   = return t 
genBal' n Emp = return Emp
genBal' n (Bind k v l r) = do klt <- choose (0, k-1)
                              kgt <- choose (k+1, 1000)
                              v' <- elements ['A' .. 'Z']
                              let ltBST = bstInsert klt v' (Bind k v l r) -- just check
                                  gtBST = bstInsert kgt v' (Bind k v l r)
                                  nBy2  = n `div` 2 
                              case isBal ltBST of                         -- do real things
                                  True  -> genBal' nBy2 ltBST
                                  False -> case isBal gtBST of
                                              True  -> genBal' nBy2 gtBST
                                              False -> genBal' nBy2 (Bind k v l r)


genBal :: Gen (BST Int Char)
genBal = genBal' 1000 (Bind 500 'M' Emp Emp)

-- such that

prop_genBal = forAll genBal isBal

-- (d) Height Balancing (** Hard **)
-- ---------------------------------

-- Rig it so that your insert and delete functions *also*
-- create balanced trees. That is, they satisfy the properties

prop_insert_bal ::  Property
prop_insert_bal = forAll (listOf genBSTadd) $ isBal . ofBSTops
-- >
prop_delete_bal ::  Property
prop_delete_bal = forAll (listOf genBSTop) $ isBal . ofBSTops






-- Problem 3: Circuit Testing
-- ==========================

-- Credit: [UPenn CIS552][1]

-- For this problem, you will look at a model of circuits in Haskell.

-- Signals
-- -------

-- A *signal* is a list of booleans.

newtype Signal = Sig [Bool]

-- By convention, all signals are infinite. We write a bunch of lifting
-- functions that lift boolean operators over signals.

lift0 ::  Bool -> Signal
lift0 a = Sig $ repeat a
-- >
lift1 ::  (Bool -> Bool) -> Signal -> Signal
lift1 f (Sig s) = Sig $ map f s
-- >
lift2 ::  (Bool -> Bool -> Bool) -> (Signal, Signal) -> Signal
lift2 f (Sig xs, Sig ys) = Sig $ zipWith f xs ys
-- >
lift22 :: (Bool -> Bool -> (Bool, Bool)) -> (Signal, Signal) -> (Signal,Signal)
lift22 f (Sig xs, Sig ys) =
  let (zs1,zs2) = unzip (zipWith f xs ys)
  in (Sig zs1, Sig zs2)
-- >
lift3 :: (Bool->Bool->Bool->Bool) -> (Signal, Signal, Signal) -> Signal
lift3 f (Sig xs, Sig ys, Sig zs) = Sig $ zipWith3 f xs ys zs
-- >

-- Simulation
-- ----------

-- Next, we have some helpers that can help us simulate a circuit by showing
-- how it behaves over time. For testing or printing, we truncate a signal to
-- a short prefix

truncatedSignalSize = 20
-- take :: Int -> [a] -> [a]
-- take n, applied to a list xs, returns the prefix of xs of length n, or xs itself if n > length xs
truncateSig bs = take truncatedSignalSize bs
-- >
instance Show Signal where
  show (Sig s) = show (truncateSig s) ++ "..."
-- >

-- unzip :: [(a, b)] -> ([a], [b])
-- unzip transforms a list of pairs into a list of first components and a list of second components.
-- intercalate :: [a] -> [[a]] -> [a]
-- intercalate xs xss is equivalent to (concat (intersperse xs xss)). It inserts the list xs in between the lists in xss and concatenates the result.
-- forM_ :: Monad m => [a] -> (a -> m b) -> m ()
-- forM_ is mapM_ with its arguments flipped
-- transpose :: [[a]] -> [[a]]
-- The transpose function transposes the rows and columns of its argument. 
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith generalises zip by zipping with the function given as the first argument, instead of a tupling function.
trace :: [(String, Signal)] -> Int -> IO ()
trace desc count = do
  putStrLn   $ intercalate " " names
  forM_ rows $ putStrLn . intercalate " " . rowS
  where (names, wires) = unzip desc
        rows           = take count . transpose . map (\ (Sig w) -> w) $ wires
        rowS bs        = zipWith (\n b -> replicate (length n - 1) ' ' ++ (show (binary b))) names bs
-- >
-- probe one signal at a time
probe :: [(String,Signal)] -> IO ()
probe desc = trace desc 1
-- >
-- probe all the signals in 20 seconds
simulate :: [(String, Signal)] -> IO ()
simulate desc = trace desc 20

-- Testing support (QuickCheck helpers)
-- ------------------------------------

-- Next, we have a few functions that help to generate random tests

instance Arbitrary Signal where
  --arbitrary :: Arbitrary a => Gen a
  arbitrary = do
    -- use type inference to decide what is the type of x and xs
    x      <- arbitrary
    Sig xs <- arbitrary
    return $ Sig (x : xs)
-- >
-- forM :: Monad m => [a] -> (a -> m b) -> m [b]
-- forM is mapM with its arguments flipped
-- gnerate a n size of signal
arbitraryListOfSize n = forM [1..n] $ \_ -> arbitrary

-- To check whether two values are equivalent

class Agreeable a where
  (===) :: a -> a -> Bool
-- >
-- all :: (a -> Bool) -> [a] -> Bool
-- Applied to a predicate and a list, all determines if all elements of the list satisfy the predicate. 
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith generalises zip by zipping with the function given as the first argument, instead of a tupling function.
instance Agreeable Signal where
  (Sig as) === (Sig bs) =
    all (\x->x) (zipWith (==) (truncateSig as) (truncateSig bs))
-- >
instance (Agreeable a, Agreeable b) => Agreeable (a,b) where
  (a1,b1) === (a2,b2) = (a1 === a2) && (b1 === b2)
-- >
instance Agreeable a => Agreeable [a] where
  as === bs = all id (zipWith (===) as bs)
-- >

-- To convert values from boolean to higher-level integers

class Binary a where
  binary :: a -> Integer
-- >
instance Binary Bool where
  binary b = if b then 1 else 0
-- >
-- 2x when meeting False, 1+2x when True 
instance Binary [Bool] where
  binary = foldr (\x r -> (binary x) + 2 *r) 0

-- And to probe signals at specific points.

sampleAt n (Sig b) = b !! n
sampleAtN n signals = map (sampleAt n) signals
sample1 = sampleAt 0
sampleN = sampleAtN 0


-- Basic Gates
-- -----------

-- The basic gates from which we will fashion circuits can now be described.

or2 ::  (Signal, Signal) -> Signal
or2 = lift2 $ \x y -> x || y
-- >
xor2 :: (Signal, Signal) -> Signal
xor2 = lift2 $ \x y -> (x && not y) || (not x && y)
-- >
and2 :: (Signal, Signal) -> Signal
and2 = lift2 $ \x y -> x && y
-- >
imp2 ::  (Signal, Signal) -> Signal
imp2 = lift2 $ \x y -> (not x) || y
-- >multiplexer
-- the third argument is a signal indicating whether to select b1
mux :: (Signal, Signal, Signal) -> Signal
mux = lift3 (\b1 b2 select -> if select then b1 else b2)
-- >
-- If selected, the first signal will appear in the first place,otherwise the second place
demux :: (Signal, Signal) -> (Signal, Signal)
demux args = lift22 (\i select -> if select then (i, False) else (False, i)) args
-- >
-- zip :: [a] -> [b] -> [(a, b)]
-- return a signal group indicating the choosing results of using mux in two signal groups  
muxN :: ([Signal], [Signal], Signal) -> [Signal]
muxN (b1,b2,sel) = map (\ (bb1,bb2) -> mux (bb1,bb2,sel)) (zip b1 b2)
-- >
-- unzip :: [(a, b)] -> ([a], [b])
demuxN :: ([Signal], Signal) -> ([Signal], [Signal])
demuxN (b,sel) = unzip (map (\bb -> demux (bb,sel)) b)


-- Basic Signals
-- -------------

-- Similarly, here are some basic signals
-- lift0 ::  Bool -> Signal
high = lift0 True
low  = lift0 False
-- >
-- all the siginal with 1 is True, others are False
str   ::  String -> Signal
str cs = Sig $ (map (== '1') cs) ++ (repeat False)
-- >
-- insert a Bool into the signal
delay ::  Bool -> Signal -> Signal
delay init (Sig xs) = Sig $ init : xs


-- Combinational circuits
-- ----------------------

-- **NOTE** When you are asked to implement a circuit, you must **ONLY** use
-- the above gates or smaller circuits built from the gates.

-- For example, the following is a *half-adder* (that adds a carry-bit to a
-- single bit).

halfadd :: (Signal, Signal) -> (Signal, Signal)
halfadd (x,y) = (sum,cout)
  where sum   = xor2 (x, y)
        cout  = and2 (x, y)

-- Here is a simple property about the half-adder

prop_halfadd_commut b1 b2 =
  halfadd (lift0 b1, lift0 b2) === halfadd (lift0 b2, lift0 b1)

-- We can use the half-adder to build a full-adder
-- extra information of cin
fulladd :: (Signal, Signal, Signal) -> (Signal, Signal)
fulladd (cin, x, y) = (sum, cout)
  where (sum1, c1)  = halfadd (x,y)
        (sum, c2)   = halfadd (cin, sum1)
        cout        = xor2 (c1,c2)
-- >
test1a = probe [("cin",cin), ("x",x), ("y",y), ("  sum",sum), ("cout",cout)]
  where cin        = high
        x          = low
        y          = high
        (sum,cout) = fulladd (cin, x, y)

-- and then an n-bit adder
-- can process many bits of signals to add with a cin
bitAdder :: (Signal, [Signal]) -> ([Signal], Signal)
bitAdder (cin, [])   = ([], cin)
bitAdder (cin, x:xs) = (sum:sums, cout)
  where (sum, c)     = halfadd (cin,x)
        (sums, cout) = bitAdder (c,xs)
-- >
test1 = probe [("cin",cin), ("in1",in1), ("in2",in2), ("in3",in3), ("in4",in4),
               ("  s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), ("c",c)]
  where
    cin = high
    in1 = high
    in2 = high
    in3 = low
    in4 = high
    ([s1,s2,s3,s4], c) = bitAdder (cin, [in1,in2,in3,in4])

-- The correctness of the above circuit is described by the following property
-- that compares the behavior of the circuit to the *reference implementation*
-- which is an integer addition function
-- in the left, it is the integer value of the whole binary number after bitAdder
-- in the right, it is the sum of two integer value of two binary number
prop_bitAdder_Correct ::  Signal -> [Bool] -> Bool
prop_bitAdder_Correct cin xs =
  binary (sampleN out ++ [sample1 cout]) == binary xs + binary (sample1 cin)
  where (out, cout) = bitAdder (cin, map lift0 xs)

-- Finally, we can use the bit-adder to build an adder that adds two N-bit numbers
-- add two bits of signals
adder :: ([Signal], [Signal]) -> [Signal]
adder (xs, ys) =
   let (sums,cout) = adderAux (low, xs, ys)
   in sums ++ [cout]
   where
     adderAux (cin, [], [])     = ([], cin)
     adderAux (cin, x:xs, y:ys) = (sum:sums, cout)
                                  where (sum, c) = fulladd (cin,x,y)
                                        (sums,cout) = adderAux (c,xs,ys)
     adderAux (cin, [], ys)     = adderAux (cin, [low], ys)
     adderAux (cin, xs, [])     = adderAux (cin, xs, [low])
-- >
test2 = probe [ ("x1", x1), ("x2",x2), ("x3",x3), ("x4",x4),
                (" y1",y1), ("y2",y2), ("y3",y3), ("y4",y4),
                (" s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), (" c",c) ]
  where xs@[x1,x2,x3,x4] = [high,high,low,low]
        ys@[y1,y2,y3,y4] = [high,low,low,low]
        [s1,s2,s3,s4,c]  = adder (xs, ys)

-- And we can specify the correctness of the adder circuit by

prop_Adder_Correct ::  [Bool] -> [Bool] -> Bool
prop_Adder_Correct l1 l2 =
  binary (sampleN sum) == binary l1 + binary l2
  where sum = adder (map lift0 l1, map lift0 l2)

-- Problem: Subtraction
-- --------------------

-- 1. Using `prop_bitAdder_Correct` as a model, write a speciﬁcation for a
-- single-bit subtraction function that takes as inputs a N-bit binary
-- number and a single bit to be subtracted from it and yields as
-- outputs an N-bit binary number. Subtracting one from zero should
-- yield zero.

-- checkBorrow is the subtraction used by bitSubtractor， right is the one used by -
prop_bitSubtractor_Correct ::  Signal -> [Bool] -> Bool
prop_bitSubtractor_Correct cin xs =
  checkBorrow == subRes 
  where (out, bout) = bitSubtractor (cin, map lift0 xs) 
        o1 = sampleN out -- result of the subtraction
        b1 = binary xs -- convert input bool arr to int
        b2 = binary $ sample1 cin -- convert signal to int (so 0, 1)
        subRes = if b1 >= b2 then b1 - b2 else 0 -- b1 can be less than b2
        hasBorrowed = sample1 bout
        checkBorrow = if hasBorrowed then
                        binary $ map not o1 -- flip signals, convert
                      else
                        binary o1 -- just convert output to int

-- 2. Using the `bitAdder` circuit as a model, deﬁne a `bitSubtractor`
-- circuit that implements this functionality and use QC to check that
-- your behaves correctly.

--First, I define a `not` function for signals, `not2`

not2 :: Signal -> Signal
not2 = lift1 (\x -> not x)

--Now, to implement a half subtractor, just like you implemented a half adder above

halfSub :: (Signal, Signal) -> (Signal, Signal)
halfSub (x, y) = (diff, borr)
  where diff   = xor2 (x, y)
        borr   = and2 ((not2 x), y)

--Finally, we can piece things together and make the full `bitSubtractor` function, which is prety much identical to `bitAdder`. 
--We just need to take care of the case of subtracting one from zero.

bitSubtractor :: (Signal, [Signal]) -> ([Signal], Signal)
bitSubtractor (cin, [])   = ([], cin)
bitSubtractor (cin, x:xs) = (sub:subs, cout)
  where (sub, c)     = halfSub (x, cin)
        (subs, cout) = bitSubtractor (c, xs)


test3 = probe [("cin",cin), ("in1",in1), ("in2",in2), ("in3",in3), ("in4",in4),
               ("  s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), ("c",c)]
  where
    cin = high
    in1 = low
    in2 = high
    in3 = low
    in4 = high
    ([s1,s2,s3,s4], c) = bitSubtractor (cin, [in1,in2,in3,in4])

test4 = probe [("cin",cin), ("in1",in1), ("in2",in2), ("in3",in3), ("in4",in4),
               ("  s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), ("c",c)]
  where
    cin = high
    in1 = low
    in2 = low
    in3 = low
    in4 = low
    ([s1,s2,s3,s4], c) = bitSubtractor (cin, [in1,in2,in3,in4])

test5 = probe [("cin",cin), ("in1",in1), ("in2",in2), ("in3",in3), ("in4",in4),
               ("  s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), ("c",c)]
  where
    cin = high
    in1 = low
    in2 = low
    in3 = low
    in4 = high
    ([s1,s2,s3,s4], c) = bitSubtractor (cin, [in1,in2,in3,in4])


-- Problem: Multiplication
-- -----------------------

-- 3. Using `prop_Adder_Correct` as a model, write down a QC speciﬁcation
-- for a `multiplier` circuit that takes two binary numbers of arbitrary
-- width as input and outputs their product.

prop_Multiplier_Correct ::  [Bool] -> [Bool] -> Bool
prop_Multiplier_Correct as bs =
  binary (sampleN prod) == binary as * binary bs
  where prod = multiplier (map lift0 as, map lift0 bs)

-- 4. Deﬁne a `multiplier` circuit and check that it satisﬁes your
-- speciﬁcation. (Looking at how adder is deﬁned will help with this,
-- but you’ll need a little more wiring. To get an idea of how the
-- recursive structure should work, think about how to multiply two
-- binary numbers on paper.)

multiplier :: ([Signal], [Signal]) -> [Signal]
multiplier ([], _) = []
multiplier (_, []) = []
multiplier (xs, ys@(y:yy)) = take resultbits $ adder (xsy, [low] ++ multiplier (xs, yy))
  where (xsy, dump) = demuxN (xs, y)
        resultbits = length xs + length ys


test6 = probe [ ("x1", x1),
                (" y1",y1),
                (" s1",s1), ("s2",s2) ]
  where xs@[x1] = [high]
        ys@[y1] = [high]
        [s1,s2] = multiplier (xs, ys)

test7 = probe [ ("x1", x1), ("x2",x2), ("x3",x3),
                (" y1",y1), ("y2",y2), ("y3",y3),
                (" s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), ("s5",s5), ("s6",s6) ]
  where xs@[x1,x2,x3] = [high,high,high]
        ys@[y1,y2,y3] = [high,high,high]
        [s1,s2,s3,s4,s5,s6] = multiplier (xs, ys)


-- [1]: http://www.cis.upenn.edu/~bcpierce/courses/552-2008/resources/circuits.hs
