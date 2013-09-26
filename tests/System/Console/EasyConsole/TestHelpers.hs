{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.EasyConsole.TestHelpers where

import System.Console.EasyConsole.BaseType
import System.Console.EasyConsole.Parser
import System.Console.EasyConsole.Run

import Test.Framework
import Test.HUnit


paramRun
  :: ParamSpec spec
  => spec a
  -> [String]
  -> ParseResult a
paramRun param args = parseArgs args $
  mkDefaultApp (liftParam param) ""


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

getSuccessProp
  :: ([String] -> ParseResult Int)
  -> (Int -> [String])
  -> Positive Int
  -> Bool
getSuccessProp parser repr = prop where
  prop (Positive i) = (Right i ==) $ parser $ repr i