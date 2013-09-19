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
  commands = map foo parsers

commandParser :: (Arg -> a) -> ParserSpec a
commandParser = liftParam . StdArgParam Mandatory Pos "command"

foo :: (Arg, CmdLineApp a) -> SpecialFlag a
foo (arg, subapp) = (parser, action) where
  parser = commandParser (== arg)
  action app args = subapp 

  
