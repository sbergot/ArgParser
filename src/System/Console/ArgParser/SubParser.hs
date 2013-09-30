{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Subparsers allows the creation of complex command line
applications organized around commands.
-}

module System.Console.ArgParser.SubParser (
    mkSubParser
  , mkSubParserWithName
  ) where

import           System.Console.ArgParser.BaseType
import           System.Console.ArgParser.Params
import           System.Console.ArgParser.Parser
import           System.Console.ArgParser.Run
import           System.Environment

-- | Create a parser composed of a list of subparsers.
--
--   Each subparser is associated with a command which the user
--   must type to activate.
mkSubParser :: [(Arg, CmdLnInterface a)] -> IO (CmdLnInterface a)
mkSubParser parsers = do
  name <- getProgName
  return $ mkSubParserWithName name parsers

-- | Same that "mkSubParser" but allows a custom name
mkSubParserWithName :: String -> [(Arg, CmdLnInterface a)] -> CmdLnInterface a
mkSubParserWithName name parsers = CmdLnInterface
  parser
  cmdSpecialFlags
  name
  Nothing
  Nothing
 where
  parser = liftParam EmptyParam
  cmdSpecialFlags = commands ++ defaultSpecialFlags
  commands = map mkSpecialFlag parsers

commandParser :: ArgParser a -> ParserSpec a
commandParser = liftParam . StdArgParam Mandatory Pos "command"

mkSpecialFlag :: (Arg, CmdLnInterface a) -> SpecialFlag a
mkSpecialFlag (arg, subapp) = (parser, action) where
  parser = commandParser . SingleArgParser $ Right . (== arg)
  action _ (posargs, flagargs) =
    parseNiceArgs (drop 1 posargs, flagargs) subapp


data EmptyParam a = EmptyParam


instance ParamSpec EmptyParam where
  getParser _ = error "impossible"
  getParamDescr _ = []
