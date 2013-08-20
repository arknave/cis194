{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Buffer
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl = jl
(+++) jl Empty = jl
(+++) m n = Append (tag m <> tag n) m n

tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single m a) = Just a
indexJ _ (Single m a) = Nothing
indexJ i (Append v m n)
  | i < 0 = Nothing
  | i < split = indexJ i m
  | otherwise = indexJ (i - split) n
  where split = getSize . size . tag $ m

dropJ:: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ _ (Single _ _) = Empty
dropJ i l@(Append v m n)
  | i <= 0 = l
  | i >= split = (dropJ (i - split) n)
  | otherwise = (dropJ i m) +++ n
  where split = getSize . size . tag $ m

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ _ Empty = Empty
takeJ _ s@(Single _ _) = s
takeJ i l@(Append v m n)
  | i <= 0 = Empty
  | i >= split = m +++ (takeJ (i - split) n)
  | otherwise = (takeJ i m)
  where split = getSize . size . tag $ m

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ m n) = toString m ++ toString n
  fromString s = Single ((scoreString s), Size 1) s
  line n jl    = indexJ n jl
  replaceLine n l b = takeJ (n-1) b +++ (fromString l) +++ dropJ n b
  numLines Empty = 0
  numLines (Single (sc, si) _)   = getSize si
  numLines (Append (sc, si) _ _) = getSize si
  value Empty  = 0
  value (Single (sc, si) _)   = getScore sc
  value (Append (sc, si) _ _) = getScore sc
