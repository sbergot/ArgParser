{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.ArgParser.ParserTest where
import System.Console.ArgParser.Parser

import System.Console.ArgParser.BaseType
import System.Console.ArgParser.QuickParams
import System.Console.ArgParser.TestHelpers

import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

data MyTest = MyTest Int Int
  deriving (Eq, Show)

myTestParser :: ParserSpec MyTest
myTestParser = MyTest
  `parsedBy` reqPos "pos1"
  `andBy` reqPos "pos2"

prop_parse
  :: Positive Int
  -> Positive Int
  -> Bool
prop_parse (Positive i) (Positive j) =
  Right (MyTest i j) == specRun myTestParser [show i, show j]