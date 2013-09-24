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
  mkDefaultApp (liftParam param) "test"

test_flagparser :: Assertion
test_flagparser = do
  let parser = paramRun (boolFlag "test")
  assertEqual (Right True) $ parser ["--test"]
  assertEqual (Right True) $ parser ["--te"]
  assertEqual (Right True) $ parser ["-t"]
  assertEqual (Right False) $ parser ["-b"]
  assertEqual (Right False) $ parser ["test"]