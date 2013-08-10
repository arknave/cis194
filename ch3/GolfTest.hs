module GolfTest where

import Golf
import Test.HUnit

testSkips :: Test
testSkips = TestList $ map (TestCase)
  [
    skips "ABCD" @?= ["ABCD", "BD", "C", "D"]
  , skips "hello!" @?= ["hello!", "el!", "l!", "l", "o", "!"]
  , skips [1] @?= [[1]]
  , skips [True, False] @?= [[True, False], [False]]
  --, (skips []) @?= [] -- This is really annoying, it won't compile because it
  --doesn't know what type the empty list is. How should I fix it?
  ]

testLocalMaxima :: Test
testLocalMaxima = TestList $ map (TestCase)
  [
    localMaxima [2,9,5,6,1] @?= [9, 6]
  , localMaxima [2, 3, 4, 1, 5] @?= [4]
  , localMaxima [1..5] @?= []
  ]

testHistogram :: Test
testHistogram = TestList $ map (TestCase)
  [
    histogram [1,1,1,5]               @?= " *        \n *        \n *   *    \n==========\n0123456789\n"
  , histogram [1,4,5,4,6,6,3,4,2,4,9] @?= "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"
  , histogram [3, 5]                  @?= "   * *    \n==========\n0123456789\n"
  ]
