{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.ArgParser.ParserTest where
import System.Console.ArgParser.Parser

import System.Console.ArgParser.BaseType
import System.Console.ArgParser.QuickParams
import System.Console.ArgParser.TestHelpers

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