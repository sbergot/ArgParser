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

assertFail :: ParseResult a -> Assertion
assertFail res = case res  of
  Left _ -> return ()
  _      -> assertFailure "expected parsing to fail."

assertSuccess
  :: (Show a, Eq a)
  => a
  -> ParseResult a
  -> Assertion
assertSuccess val res = case res  of
  Left _       -> assertFailure "parsing failed"
  Right resval -> assertEqual val resval

test_boolFlag :: Assertion
test_boolFlag = do
  let parser = paramRun (boolFlag "test")
  assertSuccess True $ parser ["--test"]
  assertSuccess True $ parser ["--te"]
  assertSuccess True $ parser ["-t"]
  assertSuccess False $ parser []
  assertFail $ parser ["-t", "arg"]

intParser :: [String] -> ParseResult Int
intParser = paramRun $ reqPos "test"

prop_reqPosSuccess :: Positive Int -> Bool
prop_reqPosSuccess (Positive i) = Right i == parsed where
  parsed = intParser [show i]
 
test_reqPos :: Assertion
test_reqPos = do
  assertFail $ intParser ["--test"]
  assertFail $ intParser ["foo"]
  assertFail $ intParser []