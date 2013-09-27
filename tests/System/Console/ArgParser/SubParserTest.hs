{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.ArgParser.SubParserTest where
import System.Console.ArgParser.SubParser

import System.Console.ArgParser.BaseType
import System.Console.ArgParser.QuickParams
import System.Console.ArgParser.Run
import System.Console.ArgParser.Parser
import System.Console.ArgParser.TestHelpers

import Test.Framework
import Test.HUnit

{-# ANN module "HLint: ignore Use camelCase" #-}

data MyTest =
  MyCons1 Int Int |
  MyCons2 Int
  deriving (Eq, Show)

myTestParser :: CmdLineApp MyTest
myTestParser = mkSubParserWithName "subparser"
  [ ("A", mkDefaultApp
    (MyCons1 `parsedBy` reqPos "pos1" `andBy` reqPos "pos2") "A")
  , ("B", mkDefaultApp
    (MyCons2 `parsedBy` reqPos "pos1") "B") 
  ]

test_subparser :: Assertion
test_subparser = do
  assertSuccess (MyCons1 1 2) $ parseArgs ["A", "1", "2"] myTestParser
  assertSuccess (MyCons2 3) $ parseArgs ["B", "3"] myTestParser