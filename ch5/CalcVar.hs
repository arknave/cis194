{-# LANGUAGE FlexibleInstances #-}
module CalcVar where

import Control.Monad
import Calc
import qualified Data.Map as M

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
        deriving (Show, Eq) 

instance Expr VarExprT where
  add a b = Add a b
  mul a b = Mul a b
  lit a   = Lit a

instance HasVars VarExprT where
  var s = Var s

instance Expr (M.Map String Integer -> Maybe Integer) where
  add a b m = liftM2 (+) (a m) (b m)
  mul a b m = liftM2 (*) (a m) (b m)
  lit a   = const $ Just a

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = M.lookup s

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
