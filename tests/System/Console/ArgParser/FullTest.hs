{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.ArgParser.FullTest where
import System.Console.ArgParser.Parser
import System.Console.ArgParser.Run

import System.Console.ArgParser.BaseType
import System.Console.ArgParser.QuickParams
import System.Console.ArgParser.TestHelpers

import Test.Framework
import qualified Test.HUnit as H

{-# ANN module "HLint: ignore Use camelCase" #-}

data MyTest = MyTest Bool Bool [String]
    deriving (Show, Eq)

myTestParser :: ParserSpec MyTest
myTestParser = MyTest
    `parsedBy` boolFlag "common-mode"
    `andBy`    boolFlag "docbook-mode"
    `andBy`    posArgs  "files" [] (\xs x -> xs ++ [x])


myTestApp :: CmdLnInterface MyTest
myTestApp = mkDefaultApp myTestParser "MyTestParser"

testparse :: [String] -> ParseResult MyTest
testparse args = parseArgs args myTestApp

checkParse :: [String] -> MyTest -> H.Assertion
checkParse args expected = behavior testparse [(willSucceed expected, args)]

expectFail :: [String] -> H.Assertion
expectFail args = behavior testparse [(willFail, args)]

test_base :: H.Assertion
test_base = checkParse
    ["1", "2", "3"]
    (MyTest False False ["1", "2", "3"])

test_pos_after_flag :: H.Assertion
test_pos_after_flag = checkParse
    ["--common-mode", "1", "2", "3"]
    (MyTest True False ["1", "2", "3"])

test_unknown_flag :: H.Assertion
test_unknown_flag = expectFail ["--unknown", "1", "2", "3"]

test_help_with_pos :: H.Assertion
test_help_with_pos = expectFail ["--help", "1", "2", "3"]
