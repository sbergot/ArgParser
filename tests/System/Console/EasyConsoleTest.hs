{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.EasyConsoleTest where
import System.Console.EasyConsole


import Test.Framework

test_nonEmpty = do assertEqual [1] (myReverse [1])
                   assertEqual [3,2,1] (myReverse [1,2,3])

test_empty = assertEqual ([] :: [Int]) (myReverse [])

prop_reverse :: [Int] -> Bool
prop_reverse xs = xs == (myReverse (myReverse xs))

myReverse :: [a] -> [a]
myReverse _ = []
--myReverse (x:xs) = (myReverse xs) ++ [x]