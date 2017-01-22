{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.ArgParser.TestHelpers where

import System.Console.ArgParser.BaseType
import System.Console.ArgParser.Parser
import System.Console.ArgParser.Run

import Test.Framework
import qualified Test.HUnit as H


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

willFail :: Show a => ParseResult a -> H.Assertion
willFail res = case res  of
  Left _    -> return ()
  Right val -> assertFailure $
    "\nexpected parsing to fail but got " ++ show val

willSucceed
  :: (Show a, Eq a)
  => a
  -> ParseResult a
  -> H.Assertion
willSucceed val res = case res  of
  Left msg     -> assertFailure $ "\nparsing failed: " ++ msg
  Right resval -> assertEqual val resval

behavior
  :: ([String] -> ParseResult a)
  -> [(ParseResult a -> H.Assertion, [String])]
  -> H.Assertion
behavior parser candidates = sequence_ assertions where
  (preds, args) = unzip candidates
  results = map parser args
  assertions = zipWith ($) preds results

getIntSuccessProp
  :: (Show a, Num a, Eq a)
  => ([String] -> ParseResult a)
  -> (a -> [String])
  -> Positive a
  -> Bool
getIntSuccessProp parser repr = prop where
  prop (Positive i) = (Right i ==) $ parser $ repr i

getMaybeIntSuccessProp
  :: (Eq a)
  => ([String] -> ParseResult (Maybe a))
  -> (a -> [String])
  -> Positive a
  -> Bool
getMaybeIntSuccessProp parser repr = prop where
  prop (Positive i) = (Right (Just i) ==) $ parser $ repr i

getStrSuccessProp
  :: ([String] -> ParseResult String)
  -> String
  -> Property
getStrSuccessProp parser = prop where
  prop str = (take 1 str /= "-") ==> Right str == parser [str]

getIntSumSuccessProp
  :: ([String] -> ParseResult Int)
  -> [String]
  -> NonEmptyList (Positive Int)
  -> Bool
getIntSumSuccessProp parser prefix = prop where
  prop (NonEmpty positives) = (Right expected ==) $ parser args where
    unpos (Positive i) = i
    expected = sum $ map unpos positives
    args = prefix ++ map (show . unpos) positives