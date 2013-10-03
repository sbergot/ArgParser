{-# OPTIONS_GHC -F -pgmF htfpp #-}
module System.Console.ArgParser.ArgsProcessTest where

import           Data.Map                               as M
import           System.Console.ArgParser.ArgsProcess
import           Test.Framework
import qualified Test.HUnit as H

{-# ANN module "HLint: ignore Use camelCase" #-}

test_empty :: H.Assertion
test_empty = assertEqual ([], M.empty) $ preprocess []

test_pos :: H.Assertion
test_pos = assertEqual (["1", "2", "3"], M.empty) $ preprocess ["1", "2", "3"]

test_single_flag :: H.Assertion
test_single_flag =
  assertEqual ([], M.fromList [("f", ["1", "2", "3"])]) $
  preprocess ["-f", "1", "2", "3"]

test_single_flag_long_form :: H.Assertion
test_single_flag_long_form =
  assertEqual ([], M.fromList [("foo", ["1", "2", "3"])]) $
  preprocess ["--foo", "1", "2", "3"]

test_multiple_flag :: H.Assertion
test_multiple_flag =
  assertEqual ([], M.fromList
    [ ("f", ["1", "2", "3"])
    , ("b", ["7", "8"])
    ]) $
  preprocess ["-f", "1", "2", "3", "-b", "7", "8"]

test_multiple_flag_short_form :: H.Assertion
test_multiple_flag_short_form =
  assertEqual ([], M.fromList
    [ ("f", [])
    , ("b", ["7", "8"])
    ]) $
  preprocess ["-fb", "7", "8"]

test_mix :: H.Assertion
test_mix =
  assertEqual (["bar"], M.fromList [("b", ["7", "8"])]) $
  preprocess ["bar", "-b", "7", "8"]

test_repeatFlag :: H.Assertion
test_repeatFlag =
  assertEqual ([], M.fromList [("b", ["7", "8", "11", "12"])]) $
  preprocess ["-b", "7", "-b", "8", "-b", "11", "12"]
