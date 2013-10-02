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

test_optFlagUsage :: Assertion
test_optFlagUsage = checkFmt (optFlag 0 "foo" :: StdArgParam Int)
  "[-f  FOO]"
  "-f, --foo  FOO"

test_optPosUsage :: Assertion
test_optPosUsage = checkFmt (optPos 0 "foo" :: StdArgParam Int)
  "[foo]"
  "foo"

test_reqFlagArgsUsage :: Assertion
test_reqFlagArgsUsage = checkFmt (reqFlagArgs "foo" 0 (+) :: StdArgParam Int)
  "-f  [FOO...]"
  "-f, --foo  [FOO...]"

test_optFlagArgsUsage :: Assertion
test_optFlagArgsUsage = checkFmt (optFlagArgs 0 "foo" 0 (+) :: StdArgParam Int)
  "[-f  [FOO...]]"
  "-f, --foo  [FOO...]"

test_posArgsUsage :: Assertion
test_posArgsUsage = checkFmt (posArgs "foo" 0 (+) :: StdArgParam Int)
  "[foo...]"
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
  , "usage : test foo bar [-h] [--version]"
  , ""
  , "mandatory arguments:"
  , " foo"
  , " bar"
  , ""
  , "optional arguments:"
  , " -h, --help                    show this help message and exit"
  , " --version                     print the program version and exit"
  , ""
  ])
  $ showCmdLineAppUsage defaultFormat myTestParser

data MyDescrTest = MyDescrTest Int Int Int Int
  deriving (Eq, Show)

myDescrTestParser :: CmdLnInterface MyDescrTest
myDescrTestParser = mkDefaultApp (MyDescrTest
  `parsedBy` reqPos "foo" `Descr` "the foo description"
  `andBy` reqPos "bar" `Descr` "the bar description"
  `andBy` reqPos "baz"
    `Descr` "the baz description wich can be very very very very very very long"
  `andBy` reqPos "bazazazazazazazazazazazazazazazazazazazazaz"
    `Descr` "the bazaz description")
  "test" `setAppDescr` "application description"
 `setAppEpilog` "application epilog"

test_DescrFormat :: Assertion
test_DescrFormat = assertEqual
  ( unlines
  [ "test"
  , "usage : test foo bar baz" ++
      " bazazazazazazazazazazazazazazazazazazazazaz" ++
      " [-h] [--version]"
  , "application description"
  , ""
  , "mandatory arguments:"
  , " foo                           the foo description"
  , " bar                           the bar description"
  , " baz                           the baz description wich can be"
  , "                               very very very very very very long"
  , " bazazazazazazazazazazazazazazazazazazazazaz"
  , "                               the bazaz description"
  , ""
  , "optional arguments:"
  , " -h, --help                    show this help message and exit"
  , " --version                     print the program version and exit"
  , ""
  , ""
  , "application epilog"
  , ""
  ])
  $ showCmdLineAppUsage defaultFormat myDescrTestParser

data MySubTest =
  MyCons1 Int Int |
  MyCons2 Int
  deriving (Eq, Show)

mySubTestParser :: CmdLnInterface MySubTest
mySubTestParser = mkSubParserWithName "subparser"
  [ ("A", mkDefaultApp
      (MyCons1 `parsedBy` reqPos "pos1" `andBy` reqPos "pos2")
        "A" `setAppDescr` "A sub description")
  , ("B", mkDefaultApp
      (MyCons2 `parsedBy` reqPos "pos1")
        "B" `setAppDescr` "B sub description") 
  ]

test_subparserFormat :: Assertion
test_subparserFormat = assertEqual
  ( unlines
  [ "subparser"
  , "usage : subparser {A,B} [-h] [--version]"
  , ""
  , "commands arguments:"
  , " {A,B}"
  , " A                             A sub description"
  , " B                             B sub description"
  , ""
  , "optional arguments:"
  , " -h, --help                    show this help message and exit"
  , " --version                     print the program version and exit"
  , ""
  ])
  $ showCmdLineAppUsage defaultFormat mySubTestParser