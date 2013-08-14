{-# LANGUAGE FlexibleInstances #-}
module Calc where

import Control.Monad
import ExprT
import Parser

class Expr a where
  add, mul:: a -> a -> a
  lit :: Integer -> a

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr ExprT where
  add a b = Add a b
  mul a b = Mul a b
  lit a = Lit a

instance Expr Integer where
  add = (+)
  mul = (*)
  lit = id 

instance Expr Bool where
  add = (||)
  mul = (&&)
  lit a
    | a <= 0 = False
    | otherwise = True

instance Expr MinMax where
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b
  lit a = MinMax a

instance Expr Mod7 where
  add (Mod7 a) (Mod7 b) = lit (a+b)
  mul (Mod7 a) (Mod7 b) = lit (a*b)
  lit a = Mod7 (flip mod a 7)

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = liftM eval (parseExp Lit Add Mul s)
