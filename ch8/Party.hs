{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Monoid
import Data.List
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL l f) = GL (emp : l) (f + empFun emp)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xl xf) (GL yl yf) = GL (xl ++ yl) (xf + yf)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = case (compare a b) of
                GT -> a
                _  -> b

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f i (Node { rootLabel = r, subForest = []}) = f r i
treeFold f i (Node { rootLabel = r, subForest = s})= f r (foldr (flip $ treeFold f) i s)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e l  = (bestWithBoss, bestNoBoss)
  where
    u = unzip l
    bestWithBoss = glCons e . mconcat . snd $ u
    bestNoBoss = mconcat $ map (uncurry moreFun) l

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . maxFun'

maxFun' :: Tree Employee -> (GuestList, GuestList)
maxFun' (Node e l) = nextLevel e (map maxFun' l)

formatList :: GuestList -> String
formatList (GL l f) = "Total fun: " ++ show f ++ "\n" ++ (unlines . sort . map empName $ l)

main = do
  file <- readFile "company.txt"
  putStr . formatList . maxFun . read $ file
