{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.ArgParser.FormatTest where
import System.Console.ArgParser.Format

import System.Console.ArgParser.BaseType
import System.Console.ArgParser.Parser
import System.Console.ArgParser.Run
import System.Console.ArgParser.QuickParams

import Test.Framework
import Test.HUnit

{-# ANN module "HLint: ignore Use camelCase" #-}

data MyTest = MyTest Int Int
  deriving (Eq, Show)

myTestParser :: CmdLineApp MyTest
myTestParser = mkDefaultApp (MyTest
  `parsedBy` reqPos "foo"
  `andBy` reqPos "bar")
  "test"

test_basicFormat :: Assertion
test_basicFormat = assertEqual
  ( unlines
  [ "test"
  , "usage : test -f FOO -b BAR"
  , ""
  , "mandatory arguments:"
  , " -f, --foo  FOO"
  , " -b, --bar  BAR"
  , ""
  ])
  $ showCmdLineAppUsage defaultFormat myTestParser