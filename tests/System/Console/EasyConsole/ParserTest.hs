{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.EasyConsole.ParserTest where
import System.Console.EasyConsole.Parser

import System.Console.EasyConsole.BaseType
import System.Console.EasyConsole.QuickParams
import System.Console.EasyConsole.TestHelpers

import Test.Framework
import Test.HUnit

{-# ANN module "HLint: ignore Use camelCase" #-}

data MyTest = MyTest Int Int
  deriving (Eq, Show)

myTestParser :: ParserSpec MyTest
myTestParser = MyTest
  `parsedBy` reqPos "pos1"
  `andBy` reqPos "pos2"

test_parse :: Assertion
test_parse = do
  assertSuccess (MyTest 1 1) $ specRun myTestParser ["1", "1"]
  assertSuccess (MyTest 2 2) $ specRun myTestParser ["2", "2"]