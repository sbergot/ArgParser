{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.ArgParser.QuickParamsTest where
import System.Console.ArgParser.QuickParams
import System.Console.ArgParser.BaseType

import System.Console.ArgParser.TestHelpers
import Test.Framework
import qualified Test.HUnit as H

{-# ANN module "HLint: ignore Use camelCase" #-}

test_boolFlag :: H.Assertion
test_boolFlag = behavior (paramRun (boolFlag "test"))
  [ (willSucceed True  ,["--test"])
  , (willSucceed True  ,["--te"])
  , (willSucceed True  ,["-t"])
  , (willSucceed False ,[])
  , (willFail          ,["-t", "arg"])
  ]

intReqParser :: [String] -> ParseResult Int
intReqParser = paramRun $ reqPos "test"

prop_reqPosSuccess :: Positive Int -> Bool
prop_reqPosSuccess = getIntSuccessProp intReqParser (\i -> [show i])

strReqParser :: [String] -> ParseResult String
strReqParser = paramRun $ reqPos "test"

prop_strReqPosSuccess :: String -> Property
prop_strReqPosSuccess = getStrSuccessProp strReqParser

floatReqParser :: [String] -> ParseResult Float
floatReqParser = paramRun $ reqPos "test"

prop_floatReqPosSuccess :: Positive Float -> Bool
prop_floatReqPosSuccess = getIntSuccessProp floatReqParser (\i -> [show i])
 
test_reqPosFailure :: H.Assertion
test_reqPosFailure = behavior intReqParser
  [ (willFail, ["--test"])
  , (willFail, ["foo"])
  , (willFail, [])
  ]

intOptParser :: [String] -> ParseResult Int
intOptParser = paramRun $ optPos 0 "test"

prop_optPosSuccess :: Positive Int -> Bool
prop_optPosSuccess = getIntSuccessProp intOptParser (\i -> [show i])
 
test_optPosFailure :: H.Assertion
test_optPosFailure = behavior intOptParser
  [ (willFail, ["foo"])
  , (willSucceed 0, [])
  ]

intReqFlagParser :: [String] -> ParseResult Int
intReqFlagParser = paramRun $ reqFlag "test"

prop_reqFlagSuccess :: Positive Int -> Bool
prop_reqFlagSuccess = getIntSuccessProp intReqFlagParser (\i -> ["-t", show i])
 
test_reqFlagFailure :: H.Assertion
test_reqFlagFailure = behavior intReqFlagParser
  [ (willFail, ["--test"])
  , (willFail, ["--test", "foo"])
  , (willFail, [])
  ]
intOptFlagParser :: [String] -> ParseResult Int
intOptFlagParser = paramRun $ optFlag 0 "test"

prop_optFlagSuccess :: Positive Int -> Bool
prop_optFlagSuccess = getIntSuccessProp intOptFlagParser (\i -> ["-t", show i])
 
test_optFlagFailure :: H.Assertion
test_optFlagFailure = behavior intOptFlagParser
  [ (willFail, ["--test"])
  , (willFail, ["--test", "foo"])
  , (willSucceed 0, [])
  ]

maybeIntOptFlagParser :: [String] -> ParseResult (Maybe Int)
maybeIntOptFlagParser = paramRun $ optFlag Nothing "test"

prop_maybeOptFlagSuccess :: Positive Int -> Bool
prop_maybeOptFlagSuccess = getMaybeIntSuccessProp maybeIntOptFlagParser (\i -> ["-t", show i])

test_maybeOptFlagFailure :: H.Assertion
test_maybeOptFlagFailure = behavior maybeIntOptFlagParser
  [ (willFail, ["--test"])
  , (willFail, ["--test", "foo"])
  , (willSucceed Nothing, [])
  , (willSucceed (Just 1), ["--test", "1"])]

intOptArgsParser :: [String] -> ParseResult Int
intOptArgsParser = paramRun $ posArgs "test" 0 (+)

prop_optPosArgsSuccess :: NonEmptyList (Positive Int) -> Bool
prop_optPosArgsSuccess = getIntSumSuccessProp intOptArgsParser []
 
test_optPosArgsFailure :: H.Assertion
test_optPosArgsFailure = behavior intOptArgsParser
  [ (willFail, ["foo"])
  , (willSucceed 0, [])
  ]

intReqFlagArgsParser :: [String] -> ParseResult Int
intReqFlagArgsParser = paramRun $ reqFlagArgs "test" 0 (+)

prop_reqFlagArgsSuccess :: NonEmptyList (Positive Int) -> Bool
prop_reqFlagArgsSuccess =
  getIntSumSuccessProp intReqFlagArgsParser ["-t"]
 
test_reqFlagArgsFailure :: H.Assertion
test_reqFlagArgsFailure = behavior intReqFlagArgsParser
  [ (willFail, ["--test", "foo"])
  , (willFail, [])
  , (willSucceed 0, ["--test"])
  ]

intOptFlagArgsParser :: [String] -> ParseResult Int
intOptFlagArgsParser = paramRun $ optFlagArgs 1 "test" 0 (+) 

prop_optFlagArgsSuccess :: NonEmptyList (Positive Int) -> Bool
prop_optFlagArgsSuccess =
  getIntSumSuccessProp intOptFlagArgsParser ["-t"]
 
test_optFlagArgsFailure :: H.Assertion
test_optFlagArgsFailure = behavior intOptFlagArgsParser
  [ (willFail, ["--test", "foo"])
  , (willSucceed 0, ["--test"])
  , (willSucceed 1, [])
  , (willSucceed 3, ["-t", "1", "-t", "2"])
  ]