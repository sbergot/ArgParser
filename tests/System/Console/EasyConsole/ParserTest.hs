{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.EasyConsole.ParserTest where
import System.Console.EasyConsole.BaseType
import System.Console.EasyConsole.Parser
import System.Console.EasyConsole.QuickParams
import System.Console.EasyConsole.Run
import Test.Framework
import Test.HUnit

{-# ANN module "HLint: ignore Use camelCase" #-}

specRun
  :: ParserSpec a
  -> [String]
  -> ParseResult a
specRun param args = parseArgs args $
  mkDefaultApp param ""

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

data MyTest = MyTest Int Int
  deriving (Eq, Show)

myTestParser :: ParserSpec MyTest
myTestParser = MyTest
  `parsedBy` reqPos "pos1"
  `andBy` reqPos "pos2"

test_parse :: Assertion
test_parse = do
  assertSuccess (MyTest 1 1) $ specRun myTestParser ["1", "1"]
  assertSuccess (MyTest 2 2) $ specRun myTestParser ["2", "2"]