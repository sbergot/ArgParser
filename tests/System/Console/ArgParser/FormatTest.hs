{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.ArgParser.FormatTest where
import System.Console.ArgParser.Format

import System.Console.ArgParser.BaseType
import System.Console.ArgParser.Parser
import System.Console.ArgParser.Params
import System.Console.ArgParser.Run
import System.Console.ArgParser.SubParser
import System.Console.ArgParser.QuickParams

import Test.Framework
import Test.HUnit

{-# ANN module "HLint: ignore Use camelCase" #-}

single :: [a] -> a
single xs = case xs of
  [x] -> x
  _   -> error "single on non-single list"
  
paramDescr
  :: ParamSpec spec
  => spec a
  -> ParamDescr
paramDescr = single . getParamDescr  

showUsage
  :: ParamSpec spec
  => spec a
  -> String
showUsage = argUsage . paramDescr

showArgFmt
  :: ParamSpec spec
  => spec a
  -> String
showArgFmt = getArgFormat . paramDescr

checkFmt
  :: ParamSpec spec
  => spec a
  -> String
  -> String
  -> Assertion
checkFmt param shortUsage longUsage = do
  assertEqual shortUsage $ showUsage param
  assertEqual longUsage $ showArgFmt param

test_boolFlagUsage :: Assertion
test_boolFlagUsage = checkFmt (boolFlag "foo")
  "[-f]"
  "-f, --foo"

test_reqFlagUsage :: Assertion
test_reqFlagUsage = checkFmt (reqFlag "foo" :: StdArgParam Int)
  "-f  FOO"
  "-f, --foo  FOO"

test_reqPosUsage :: Assertion
test_reqPosUsage = checkFmt (reqPos "foo" :: StdArgParam Int)
  "foo"
  "foo"

data MyTest = MyTest Int Int
  deriving (Eq, Show)

myTestParser :: CmdLnInterface MyTest
myTestParser = mkDefaultApp (MyTest
  `parsedBy` reqPos "foo"
  `andBy` reqPos "bar")
  "test"

test_basicFormat :: Assertion
test_basicFormat = assertEqual
  ( unlines
  [ "test"
  , "usage : test foo bar [-h] [-v]"
  , ""
  , "mandatory arguments:"
  , " foo"
  , " bar"
  , ""
  , "optional arguments:"
  , " -h, --help                    show this help message and exit"
  , " -v, --version                 print the program version and exit"
  , ""
  ])
  $ showCmdLineAppUsage defaultFormat myTestParser

data MySubTest =
  MyCons1 Int Int |
  MyCons2 Int
  deriving (Eq, Show)

mySubTestParser :: CmdLnInterface MySubTest
mySubTestParser = mkSubParserWithName "subparser"
  [ ("A", mkDefaultApp
      (MyCons1 `parsedBy` reqPos "pos1" `andBy` reqPos "pos2") "A")
  , ("B", mkDefaultApp
      (MyCons2 `parsedBy` reqPos "pos1") "B") 
  ]

test_subparserFormat :: Assertion
test_subparserFormat = assertEqual
  ( unlines
  [ "subparser"
  , "usage : subparser {A,B} [-h] [-v]"
  , ""
  , "commands arguments:"
  , " {A,B}"
  , " A"
  , " B"
  , ""
  , "optional arguments:"
  , " -h, --help                    show this help message and exit"
  , " -v, --version                 print the program version and exit"
  , ""
  ])
  $ showCmdLineAppUsage defaultFormat mySubTestParser