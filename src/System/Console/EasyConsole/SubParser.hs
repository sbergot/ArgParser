module System.Console.EasyConsole.SubParser where

import System.Console.EasyConsole.BaseType
import System.Console.EasyConsole.Params
import System.Console.EasyConsole.Parser
import System.Console.EasyConsole.Run

mkSubParser :: String -> [(Arg, CmdLineApp a)] -> CmdLineApp a
mkSubParser name parsers = CmdLineApp
  parser
  cmdSpecialFlags
  name
  Nothing
  Nothing
 where
  parser = commandParser (error "impossible")
  cmdSpecialFlags = commands ++ defaultSpecialFlags
  commands = map mkSpecialFlag parsers

commandParser :: (Arg -> a) -> ParserSpec a
commandParser = liftParam . StdArgParam Mandatory Pos "command"

mkSpecialFlag :: (Arg, CmdLineApp a) -> SpecialFlag a
mkSpecialFlag (arg, subapp) = (parser, action) where
  parser = commandParser (== arg)
  action _ (posargs, flagargs) =
    runAppWith (drop 1 posargs, flagargs) subapp

  
