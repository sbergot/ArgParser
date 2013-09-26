{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.EasyConsole.QuickParamsTest where
import System.Console.EasyConsole.QuickParams
import System.Console.EasyConsole.BaseType

import System.Console.EasyConsole.TestHelpers
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
prop_reqPosSuccess = getSuccessProp intReqParser (\i -> [show i])
 
test_reqPosFailure :: Assertion
test_reqPosFailure = do
  assertFail $ intReqParser ["--test"]
  assertFail $ intReqParser ["foo"]
  assertFail $ intReqParser []

intOptParser :: [String] -> ParseResult Int
intOptParser = paramRun $ optPos 0 "test"

prop_optPosSuccess :: Positive Int -> Bool
prop_optPosSuccess = getSuccessProp intOptParser (\i -> [show i])
 
test_optPosFailure :: Assertion
test_optPosFailure = do
  assertFail $ intOptParser ["foo"]
  assertSuccess 0 $ intOptParser []

intReqFlagParser :: [String] -> ParseResult Int
intReqFlagParser = paramRun $ reqFlag "test"

prop_reqFlagSuccess :: Positive Int -> Bool
prop_reqFlagSuccess = getSuccessProp intReqFlagParser (\i -> ["-t", show i])
 
test_reqFlagFailure :: Assertion
test_reqFlagFailure = do
  assertFail $ intReqFlagParser ["--test"]
  assertFail $ intReqFlagParser ["--test", "foo"]
  assertFail $ intReqFlagParser []

intOptFlagParser :: [String] -> ParseResult Int
intOptFlagParser = paramRun $ optFlag 0 "test"

prop_optFlagSuccess :: Positive Int -> Bool
prop_optFlagSuccess = getSuccessProp intOptFlagParser (\i -> ["-t", show i])
 
test_optFlagFailure :: Assertion
test_optFlagFailure = do
  assertFail $ intOptFlagParser ["--test"]
  assertFail $ intOptFlagParser ["--test", "foo"]
  assertSuccess 0 $ intOptFlagParser []