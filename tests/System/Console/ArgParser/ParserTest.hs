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

data MySuperTest = MySuperTest Int Int Int MyTest Int Int
  deriving (Eq, Show)

mySuperParser :: ParserSpec MySuperTest
mySuperParser = MySuperTest
 `parsedBy` reqPos "pos1"
 `andBy` reqPos "pos2"
 `andBy` reqPos "pos3"
 `andBy` (MyTest
   `subParser` reqPos "pos4"
   `andBy` reqPos "pos5")
 `andBy` reqPos "pos6"
 `andBy` reqPos "pos7"

prop_superParse
  :: Positive Int
  -> Positive Int
  -> Positive Int
  -> Positive Int
  -> Positive Int
  -> Positive Int
  -> Positive Int
  -> Bool
prop_superParse
  (Positive i1)
  (Positive i2)
  (Positive i3)
  (Positive i4)
  (Positive i5)
  (Positive i6)
  (Positive i7) =
  Right expected == result where
    expected = MySuperTest i1 i2 i3 (MyTest i4 i5) i6 i7 
    result = specRun mySuperParser $ map show
      [ i1, i2, i3, i4, i5, i6, i7]