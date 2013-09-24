{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} System.Console.EasyConsole.RunTest
import {-@ HTF_TESTS @-} System.Console.EasyConsole.FormatTest
import {-@ HTF_TESTS @-} System.Console.EasyConsoleTest
import {-@ HTF_TESTS @-} System.Console.EasyConsole.SubParserTest
import {-@ HTF_TESTS @-} System.Console.EasyConsole.QuickParamsTest
import {-@ HTF_TESTS @-} System.Console.EasyConsole.ParserTest
import {-@ HTF_TESTS @-} System.Console.EasyConsole.ArgsProcessTest

main :: IO()
main = htfMain htf_importedTests
