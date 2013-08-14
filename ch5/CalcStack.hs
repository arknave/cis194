{-# LANGUAGE FlexibleInstances #-}
module CalcStack where

import Parser
import Calc
import qualified StackVM as SVM

instance Expr SVM.Program where
  add a b = a ++ b ++ [SVM.Add]
  mul a b = a ++ b ++ [SVM.Mul]
  lit i = [SVM.PushI i]

compile :: String -> Maybe SVM.Program
compile s = (parseExp lit add mul s)
