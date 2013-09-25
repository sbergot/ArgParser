{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.EasyConsole.QuickParamsTest where
import System.Console.EasyConsole.BaseType
import System.Console.EasyConsole.Parser
import System.Console.EasyConsole.QuickParams
import System.Console.EasyConsole.Run


import Test.Framework
import Test.HUnit

{-# ANN module "HLint: ignore Use camelCase" #-}

paramRun
  :: ParamSpec spec
  => spec a
  -> [String]
  -> ParseResult a
paramRun param args = parseArgs args $
  mkDefaultApp (liftParam param) ""

assertFail :: Show a => ParseResult a -> Assertion
assertFail res = case res  of
  Left _    -> return ()
  Right val -> assertFailure $
    "expected parsing to fail but got " ++ show val

assertSuccess
  :: (Show a, Eq a)
  => a
  -> ParseResult a
  -> Assertion
assertSuccess val res = case res  of
  Left _       -> assertFailure "parsing failed"
  Right resval -> assertEqual val resval

getSuccessProp
  :: ([String] -> ParseResult Int)
  -> (Int -> [String])
  -> Positive Int
  -> Bool
getSuccessProp parser repr = prop where
  prop (Positive i) = (Right i ==) $ parser $ repr i

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