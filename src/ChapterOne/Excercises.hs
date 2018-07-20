module ChapterOne.Excercises
    ( mainOne
    ) where

import Data.Maybe (fromJust)
import Data.List (find)
import Control.Monad (mplus)
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class

-- Program Section

mainOne :: IO ()
mainOne = interpret program

data Expression = IdExp String
                | NumExp Int
                | OpExp Expression BinOp Expression
                | EseqExp Statement Expression

data Statement = CompoundStm Statement Statement
               | AssignStm String Expression
               | PrintStm [Expression]

data BinOp = Plus 
           | Minus 
           | Times 
           | Divide

-- a := 5 + 3 ; b := ( print ( a, a - 1) , 10 * a) ; print (b)
program :: Statement
program = CompoundStm 
            (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
            (CompoundStm
                (AssignStm "b" 
                    (EseqExp
                        (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                        (OpExp (NumExp 10) Times (IdExp "a"))))
                (PrintStm [IdExp "b"]))

-- Tells the maximum number of arguments of any print statement within any
-- subexpressions of a given statement.
maxArgs :: Statement -> Int
maxArgs (CompoundStm lStm rStm) = max (maxArgs lStm) (maxArgs rStm)
maxArgs (AssignStm _ expr)      = maxArgsExpr expr
maxArgs (PrintStm xs)           = max (length xs) (maximum (fmap maxArgsExpr xs))

maxArgsExpr :: Expression -> Int
maxArgsExpr (OpExp lExpr _ rExpr) = max (maxArgsExpr lExpr) (maxArgsExpr rExpr)
maxArgsExpr (EseqExp stm expr)    = max (maxArgs stm) (maxArgsExpr expr)
maxArgsExpr (IdExp _)             = 0
maxArgsExpr (NumExp _)            = 0

interpret :: Statement -> IO ()
interpret statement = evalStateT (interpStm statement) []

-- The first occurence of an id (String) is the one that will be used
type Table = [(String, Int)]

type PrintCommands = [[Int]]

lookup' :: String -> Table -> Int
lookup' s = fromJust . lookup s 

interpStm :: Statement -> StateT Table IO ()
interpStm (CompoundStm lStm rStm) = interpStm lStm  >>= (const $ interpStm rStm)
interpStm (AssignStm s expr)      = interpExpr expr >>= (\r -> modify ((:) (s, r)))
interpStm (PrintStm xs)           = traverse printInt xs >>= newLine
    where printInt x = interpExpr x >>= liftIO . putStr . (\x -> x ++ " ") . show
          newLine    = const $ liftIO (putStr "\n")

interpExpr :: Expression -> StateT Table IO Int
interpExpr (OpExp lExpr Plus   rExpr) = binop (+) lExpr rExpr
interpExpr (OpExp lExpr Minus  rExpr) = binop (-) lExpr rExpr
interpExpr (OpExp lExpr Times  rExpr) = binop (*) lExpr rExpr
interpExpr (OpExp lExpr Divide rExpr) = binop div lExpr rExpr
interpExpr (IdExp s)                  = fmap (lookup' s) get
interpExpr (NumExp x)                 = return x
interpExpr (EseqExp stm expr)         = do
    _ <- interpStm stm
    r <- interpExpr expr
    return r

binop :: (Int -> Int -> Int) 
      -> Expression
      -> Expression
      -> StateT Table IO Int
binop f lExpr rExpr = do
    lr <- interpExpr lExpr
    rr <- interpExpr rExpr
    return (f lr rr)

-- Exercises Section

type Key = String

data Tree a = Leaf | Node (Tree a) Key a (Tree a)
    deriving (Show)

empty :: Tree a
empty = Leaf

insert :: Key -> Tree a -> a -> Tree a
insert key Leaf a = Node Leaf key a Leaf
insert key (Node l k a' r) a
    | key < k   = Node (insert key l a) k a' r
    | key > k   = Node l k a' (insert key r a)
    | otherwise = Node l key a' r

member :: Key -> Tree a -> Bool
member _ Leaf = False
member key (Node l k a r)
    | key == k  = True
    | otherwise = member key l || member key r

lookupInTree :: Tree a -> Key -> Maybe a
lookupInTree Leaf _ = Nothing
lookupInTree (Node l k a r) key
    | k == key  = Just a
    | otherwise = lookupInTree l key `mplus` lookupInTree r key