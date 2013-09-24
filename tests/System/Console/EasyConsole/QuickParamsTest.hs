{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.EasyConsole.QuickParamsTest where
import System.Console.EasyConsole.BaseType
import System.Console.EasyConsole.Parser
import System.Console.EasyConsole.QuickParams
import System.Console.EasyConsole.Run


import Test.Framework
import Test.HUnit

{-# ANN module "HLint: ignore Use camelCase" #-}

quickRun
  :: ParamSpec spec
  => spec a
  -> [String]
  -> ParseResult a
quickRun param args = parseArgs args $
  mkDefaultApp (liftParam param) "test"

test_flagparser :: Assertion
test_flagparser = assertEqual (Right True) $
  quickRun (boolFlag "test") ["--test"]