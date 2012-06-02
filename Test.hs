module Main where

import Data.List (sort)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

import RealWorldHaskell

main :: IO ()
main = defaultMain tests

testPoints1 :: [Point]
testPoints1 =  [(-3,5), (2,3), (0,1), (0,0), (1,1), (1,0), (3,3), (3,0),
    (0,-3), (-3,-3), (-3,1)]

correctHull1 :: [Point]
correctHull1 = [(0.0,-3.0), (3.0,0.0), (3.0,3.0), (-3.0,5.0), (-3.0,1.0),
    (-3.0,-3.0)]

testPoints2 :: [Point]
testPoints2 = [(-3,1), (-4,1), (-1,4), (0,0), (2,2), (-1,3), (-1,2), (1,0),
    (3,-1), (-1,-1)]

correctHull2 :: [Point]
correctHull2 = [(3.0,-1.0), (2.0,2.0), (-1.0,4.0), (-4.0,1.0), (-1.0,-1.0)]

testPoints3 :: [Point]
testPoints3 = [(2.282154, 1.107697),
  (0.508136, 3.557593),
  (6.490489, 7.220862),
  (5.965358, 7.999035),
  (8.800029, 5.776057),
  (8.404778, 6.139152),
  (0.743481, 2.498024),
  (1.002515, 1.441651),
  (6.814331, 4.425290),
  (0.351636, 7.519613),
  (7.209313, 4.625614),
  (3.246308, 8.286591),
  (1.427197, 8.788250),
  (8.295495, 1.439363),
  (6.410940, 2.408661),
  (4.719379, 4.218519),
  (5.873703, 6.961398),
  (6.572547, 5.621853),
  (1.308362, 2.677804),
  (3.789837, 3.064397)]

correctHull3 :: [Point]
correctHull3 = [(2.282154, 1.107697),
  (8.295495, 1.439363),
  (8.800029, 5.776057),
  (8.404778, 6.139152),
  (5.965358, 7.999035),
  (1.427197, 8.78825),
  (0.351636, 7.519613),
  (0.508136, 3.557593),
  (0.743481, 2.498024),
  (1.002515, 1.441651)]
  
testPoints4 :: [Point]
testPoints4 = [(0, 0), (1, 1), ((-1), 1), (0.5, 0.9), (0, 0.7)]

correctHull4 :: [Point]
correctHull4 = [(0, 0), (1, 1), (-1, 1)]

tests :: [Test]
tests = 
    [testGroup "cases" $ zipWith (testCase . show) [1 :: Int ..]
       [ -- Chapter 1
         countWordsInString "" @?= 0,
         countWordsInString "one" @?= 1,
         countWordsInString "one two" @?= 2,

         countCharactersInString "" @?= 0,
         countCharactersInString "one" @?= 3,
         
         -- Chapter 2
         lastButOne "" @?= Nothing,
         lastButOne "1" @?= Nothing,
         lastButOne "123" @?= Just '2',
         
         -- Chapter 3
         toList (Nil :: List Int) @?= ([] :: [Int]),
         toList (Cons 'a' Nil) @?= "a",

         numElements [] @?= 0,
         numElements "a" @?= 1,
         numElements "ab" @?= 2,

         listMean ([] :: [Double]) @?= (0.0 :: Double),
         listMean ([1] :: [Double]) @?= (1.0 :: Double),
         listMean ([1,2] :: [Double]) @?= (1.5 :: Double),

         toPalindrome ([] :: [Int]) @?= [],
         toPalindrome "1" @?= "11",
         toPalindrome "123" @?= "123321",

         isPalindrome ([] :: [Int]) @?= True,
         isPalindrome "1" @?= True,
         isPalindrome "12" @?= False,
         isPalindrome "121" @?= True,
         isPalindrome "1221" @?= True,
         
         sortListsByLength ([[]] :: [[Int]]) @?= [[]],
         sortListsByLength ["1"] @?= ["1"],
         sortListsByLength ["1","12"] @?= ["1","12"],
         sortListsByLength ["12","1"] @?= ["1","12"],
         
         joinWithSeparator ',' [] @?= "",
         joinWithSeparator ',' [""] @?= "",
         joinWithSeparator ',' ["foo"] @?= "foo",
         joinWithSeparator ',' ["foo","bar","baz"] @?= "foo,bar,baz",
         
         treeHeight Empty @?= 0,
         treeHeight (Node "x" Empty Empty) @?= 1,
         treeHeight (Node "x" Empty (Node "y" Empty Empty)) @?= 2,
         
         calculateTurn (3, 3) (4, 4) (5, 5) @?= Straight,
         calculateTurn (3, 3) (4, 4) (4, 5) @?= TurnsLeft,
         calculateTurn (3, 3) (4, 4) (6, 5) @?= TurnsRight,
         
         turnList [] @?= [],
         turnList [(1, 1), (2, 2)] @?= [],
         turnList [(3, 3), (4, 4), (5, 5)] @?= [Straight],
         turnList [(3, 3), (4, 4), (6, 4), (5, 0)] @?= [TurnsRight,TurnsRight],
         turnList [(3, 3), (4, 4), (5, 5), (6, 6), (4, 6)] @?=
                [Straight,Straight,TurnsLeft],
                
         findP [(1,1)] @?= (1,1),
         findP [(1,10),(2,5)] @?= (2,5),
         findP [(2,1),(1,1)] @?= (1,1),
         
         sortByAngle [(0,1), (5,5), ((-5),5), ((-5),(-5)), (5,(-5))] @?=
                [(-5.0,-5.0),(5.0,-5.0),(5.0,5.0),(0.0,1.0),(-5.0,5.0)],
         
         sort (convexHull testPoints1) @?= sort correctHull1,
         sort (convexHull testPoints2) @?= sort correctHull2,
         sort (convexHull testPoints3) @?= sort correctHull3,
         sort (convexHull testPoints4) @?= sort correctHull4,
         
         -- Chapter 4
         safeHead "" @?= Nothing,
         safeHead "a" @?= Just 'a',
         safeTail "" @?= Nothing,
         safeTail "a" @?= Just "",
         safeLast "" @?= Nothing,
         safeLast "a" @?= Just 'a',
         safeInit "" @?= Nothing,
         safeInit "a" @?= Just "",
         
         splitWith id [] @?= [],
         splitWith id [True] @?= [],
         splitWith id [False] @?= [[False]],
         splitWith id [False, False] @?= [[False, False]],
         splitWith id [False, True] @?= [[False]],
         splitWith id [True, False] @?= [[False]],
         splitWith id [True, True] @?= [],
         splitWith id [True, False, False] @?= [[False, False]],
         splitWith id [False, False, True] @?= [[False, False]],
         splitWith id [True, False, True] @?= [[False]],
         splitWith id [False, True, False] @?= [[False], [False]],
         
         firstWords "   " @?= "\n",
         firstWords "\n" @?= "\n",
         firstWords "one" @?= "one\n",
         firstWords "one two" @?= "one\n",
         firstWords "one\ntwo" @?= "one\ntwo\n",
         firstWords "one two\nthree four\n" @?= "one\nthree\n",
         firstWords "one\n\n\n\ntwo" @?= "one\n\n\n\ntwo\n",
         
         transposeText "" @?= "",
         transposeText "a" @?= "a\n",
         transposeText "a\nb" @?= "ab\n",
         transposeText "ab\ncd\n" @?= "ac\nbd\n",
         transposeText "abc\nd" @?= "ad\nb\nc\n",
         transposeText "a\nb\nc\ndef" @?= "abcd\ne\nf\n",
         transposeText "hello\nworld\n" @?= "hw\neo\nlr\nll\nod\n",
         
         asInt_fold "" @?= 0,
         asInt_fold "0" @?= 0,
         asInt_fold "-" @?= 0,
         asInt_fold "101" @?= 101,
         asInt_fold "-31337" @?= -31337,
         asInt_fold "1798" @?= 1798,
         asInt_fold "-3" @?= -3,
         
         asInt_either "" @?= Right 0,
         asInt_either "0" @?= Right 0,
         asInt_either "-" @?= Right 0,
         asInt_either "101" @?= Right 101,
         asInt_either "-31337" @?= Right (-31337),
         asInt_either "1798" @?= Right 1798,
         asInt_either "-3" @?= Right (-3),
         asInt_either "b" @?= Left "non-digit 'b'",
         asInt_either "123c" @?= Left "non-digit 'c'",
         asInt_either "-123d" @?= Left "non-digit 'd'",
         
         foldConcat ([] :: [String]) @?= [],
         foldConcat [""] @?= "",
         foldConcat ["foo"] @?= "foo",
         foldConcat ["foo", "bar", "baz"] @?= "foobarbaz",
         
         takeWhileRec id [] @?= [],
         takeWhileRec id [False] @?= [],
         takeWhileRec id [True] @?= [True],
         takeWhileRec id [True, True, False] @?= [True, True],
         takeWhileRec id [True, True, False, True] @?= [True, True],
         takeWhileRec (< 3) ([1..] :: [Int]) @?= [1,2],
         
         takeWhileFold id [] @?= [],
         takeWhileFold id [False] @?= [],
         takeWhileFold id [True] @?= [True],
         takeWhileFold id [True, True, False] @?= [True, True],
         takeWhileFold id [True, True, False, True] @?= [True, True],
         takeWhileFold (< 3) ([1..] :: [Int]) @?= [1,2],
         
         groupByFold (==) ([] :: [Int]) @?= [],
         groupByFold (==) "1" @?= ["1"],
         groupByFold (==) "11" @?= ["11"],
         groupByFold (==) "112211" @?= ["11","22","11"],
         groupByFold (==) "123321" @?= ["1","2","33","2","1"],
         
         anyFold id [] @?= False,
         anyFold id [False] @?= False,
         anyFold id [True] @?= True,
         anyFold id [False, False, True] @?= True,
         
         take 5 (cycleFold "1") @?= "11111",
         take 5 (cycleFold "11") @?= "11111",
         take 5 (cycleFold "123") @?= "12312",
         
         wordsFold "one" @?= ["one"],
         wordsFold "one two" @?= ["one","two"],
         wordsFold "one   two" @?= ["one","two"],
         wordsFold "  one" @?= ["one"],
         wordsFold "one   " @?= ["one"],
         wordsFold "  one  two  " @?= ["one","two"]
       ]
    ]

    
    
    
    
    
    
    
    
    
    
    
    
    
    