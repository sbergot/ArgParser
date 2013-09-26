{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.ArgParser.QuickParamsTest where
import System.Console.ArgParser.QuickParams
import System.Console.ArgParser.BaseType

import System.Console.ArgParser.TestHelpers
import Test.Framework
import Test.HUnit

{-# ANN module "HLint: ignore Use camelCase" #-}

test_boolFlag :: Assertion
test_boolFlag = do
  let parser = paramRun (boolFlag "test")
  assertSuccess True $ parser ["--test"]
  assertSuccess True $ parser ["--te"]
  assertSuccess True $ parser ["-t"]
  assertSuccess False $ parser []
  assertFail $ parser ["-t", "arg"]

intReqParser :: [String] -> ParseResult Int
intReqParser = paramRun $ reqPos "test"

prop_reqPosSuccess :: Positive Int -> Bool
prop_reqPosSuccess = getIntSuccessProp intReqParser (\i -> [show i])
 
test_reqPosFailure :: Assertion
test_reqPosFailure = do
  assertFail $ intReqParser ["--test"]
  assertFail $ intReqParser ["foo"]
  assertFail $ intReqParser []

intOptParser :: [String] -> ParseResult Int
intOptParser = paramRun $ optPos 0 "test"

prop_optPosSuccess :: Positive Int -> Bool
prop_optPosSuccess = getIntSuccessProp intOptParser (\i -> [show i])
 
test_optPosFailure :: Assertion
test_optPosFailure = do
  assertFail $ intOptParser ["foo"]
  assertSuccess 0 $ intOptParser []

intReqFlagParser :: [String] -> ParseResult Int
intReqFlagParser = paramRun $ reqFlag "test"

prop_reqFlagSuccess :: Positive Int -> Bool
prop_reqFlagSuccess = getIntSuccessProp intReqFlagParser (\i -> ["-t", show i])
 
test_reqFlagFailure :: Assertion
test_reqFlagFailure = do
  assertFail $ intReqFlagParser ["--test"]
  assertFail $ intReqFlagParser ["--test", "foo"]
  assertFail $ intReqFlagParser []

intOptFlagParser :: [String] -> ParseResult Int
intOptFlagParser = paramRun $ optFlag 0 "test"

prop_optFlagSuccess :: Positive Int -> Bool
prop_optFlagSuccess = getIntSuccessProp intOptFlagParser (\i -> ["-t", show i])
 
test_optFlagFailure :: Assertion
test_optFlagFailure = do
  assertFail $ intOptFlagParser ["--test"]
  assertFail $ intOptFlagParser ["--test", "foo"]
  assertSuccess 0 $ intOptFlagParser []

intOptArgsParser :: [String] -> ParseResult Int
intOptArgsParser = paramRun $ posArgs "test" 0 (+)

prop_optPosArgsSuccess :: NonEmptyList (Positive Int) -> Bool
prop_optPosArgsSuccess = getIntSumSuccessProp intOptArgsParser []
 
test_optPosArgsFailure :: Assertion
test_optPosArgsFailure = do
  assertFail $ intOptArgsParser ["foo"]
  assertSuccess 0 $ intOptArgsParser []

intReqFlagArgsParser :: [String] -> ParseResult Int
intReqFlagArgsParser = paramRun $ reqFlagArgs "test" 0 (+)

prop_reqFlagArgsSuccess :: NonEmptyList (Positive Int) -> Bool
prop_reqFlagArgsSuccess =
  getIntSumSuccessProp intReqFlagArgsParser ["-t"]
 
test_reqFlagArgsFailure :: Assertion
test_reqFlagArgsFailure = do
  assertSuccess 0 $ intReqFlagArgsParser ["--test"]
  assertFail $ intReqFlagArgsParser ["--test", "foo"]
  assertFail $ intReqFlagArgsParser []

intOptFlagArgsParser :: [String] -> ParseResult Int
intOptFlagArgsParser = paramRun $ optFlagArgs 1 "test" 0 (+) 

prop_optFlagArgsSuccess :: NonEmptyList (Positive Int) -> Bool
prop_optFlagArgsSuccess =
  getIntSumSuccessProp intOptFlagArgsParser ["-t"]
 
test_optFlagArgsFailure :: Assertion
test_optFlagArgsFailure = do
  assertSuccess 0 $ intOptFlagArgsParser ["--test"]
  assertFail $ intOptFlagArgsParser ["--test", "foo"]
  assertSuccess 1 $ intOptFlagArgsParser []
  assertSuccess 3 $ intOptFlagArgsParser ["-t", "1", "-t", "2"]