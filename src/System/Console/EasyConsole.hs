module System.Console.EasyConsole where

import System.Console.EasyConsole.ArgsProcess

data Parser a = Parser (Args -> a)