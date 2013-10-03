{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.ArgParser.SubParserTest where
import System.Console.ArgParser.SubParser

import System.Console.ArgParser.BaseType
import System.Console.ArgParser.QuickParams
import System.Console.ArgParser.Run
import System.Console.ArgParser.Parser
import System.Console.ArgParser.TestHelpers

import Test.Framework
import qualified Test.HUnit as H

{-# ANN module "HLint: ignore Use camelCase" #-}

data MyTest =
  MyCons1 Int Int |
  MyCons2 Int
  deriving (Eq, Show)

myTestParser :: CmdLnInterface MyTest
myTestParser = mkSubParserWithName "subparser"
  [ ("A", mkDefaultApp
    (MyCons1 `parsedBy` reqPos "pos1" `andBy` reqPos "pos2") "A")
  , ("B", mkDefaultApp
    (MyCons2 `parsedBy` reqPos "pos1") "B") 
  ]

test_subparser :: H.Assertion
test_subparser = behavior (`parseArgs` myTestParser)
  [ (willSucceed (MyCons1 1 2), ["A", "1", "2"])
  , (willSucceed (MyCons2 3), ["B", "3"])
  , (willFail, ["3"])
  ]