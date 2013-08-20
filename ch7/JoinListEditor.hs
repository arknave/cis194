module Main where

import Scrabble
import Sized
import Editor
import JoinList

main = runEditor editor $ Single (Score 1, Size 1) "a"
