{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int deriving (Eq, Ord, Num, Show)

score :: Char -> Score
score c
  | toUpper c `elem` "QZ" = Score 10
  | toUpper c `elem` "JX" = Score 8
  | toUpper c `elem` "K" =  Score 5
  | toUpper c `elem` "FHVWY" = Score 4
  | toUpper c `elem` "BCMP" = Score 3
  | toUpper c `elem` "DG" = Score 2
  | toUpper c `elem` "AEIOURSTLN" = Score 1
  | otherwise = Score 0

scoreString :: String -> Score
scoreString = mconcat . (map score)

getScore :: Score -> Int
getScore (Score n) = n

instance Monoid Score where
  mempty = Score 0
  mappend = (+)
