module System.Console.EasyConsole.SubParser
 ( mkSubParser
 ) where

import           System.Console.EasyConsole.BaseType
import           System.Console.EasyConsole.Params
import           System.Console.EasyConsole.Parser
import           System.Console.EasyConsole.Run
import           System.Environment

mkSubParser :: [(Arg, CmdLineApp a)] -> IO (CmdLineApp a)
mkSubParser parsers = do
  name <- getProgName
  return $ mkSubParserWithName name parsers

mkSubParserWithName :: String -> [(Arg, CmdLineApp a)] -> CmdLineApp a
mkSubParserWithName name parsers = CmdLineApp
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
    parseArgs (drop 1 posargs, flagargs) subapp


