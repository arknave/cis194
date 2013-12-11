{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Data.List
import Control.Monad
import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle f = do
    alist <- dice (min 3 $ attackers f)
    blist <- dice (min 2 $ defenders f)
    let zlist = zip (sort alist) (sort blist)
    let atklen = length . filter (\(a, b) -> a > b) $ zlist
    return $ Battlefield (attackers f - atklen) (defenders f + atklen - (length zlist))

invade :: Battlefield -> Rand StdGen Battlefield
invade f 
    | attackers f < 2 || defenders f < 1 = return f
    | otherwise = (battle f) >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb f = do
    results <- replicateM 1000 (invade f)
    let r = (fromIntegral . length . filter (\x -> (defenders x) == 0) $ results) 
    let all = (fromIntegral . length $ results)
    return (r / all)

exactSuccessProb :: Battlefield -> Double
exactSuccessProb f
    | d < 1 = 1.0
    | a < 2 = 0.0
    | otherwise = ((exactSuccessProb $ Battlefield a (d-2)) + (exactSuccessProb $ Battlefield (a-1) (d-1)) + (exactSuccessProb $ Battlefield (a-2) d)) / 3
    where
        d = defenders f
        a = attackers f
