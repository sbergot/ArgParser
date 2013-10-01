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

import qualified Data.List                         as L
import qualified Data.Map                          as M
import           Data.Maybe
import           System.Console.ArgParser.BaseType
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
  parser cmdSpecialFlags name Nothing Nothing
 where
  parser = liftParam EmptyParam
  cmdSpecialFlags = command:defaultSpecialFlags
  command = mkSpecialFlag parsers

mkSpecialFlag :: [(Arg, CmdLnInterface a)] -> SpecialFlag a
mkSpecialFlag subapps = (parser, action) where
  parser = liftParam $ CommandParam cmdMap id
  action _ (posargs, flagargs) =
    case listToMaybe posargs >>= flip M.lookup cmdMap of
      Nothing     -> error "impossible"
      Just subapp -> parseNiceArgs (drop 1 posargs, flagargs) subapp
  cmdMap = M.fromList subapps

data EmptyParam a = EmptyParam

instance ParamSpec EmptyParam where
  getParser _ = error "impossible"
  getParamDescr _ = []

data CommandParam appT resT = CommandParam 
  (M.Map String (CmdLnInterface appT))
  (Bool -> resT)

instance ParamSpec (CommandParam resT) where
  getParser (CommandParam cmdMap convert) = Parser cmdParser where
    cmdParser (pos, flags) = case pos of
      []    -> (Left "No command provided", (pos, flags))
      arg:_ -> (Right $ convert isMatch, ([], M.empty)) where
        isMatch = arg `M.member` cmdMap

  getParamDescr (CommandParam cmdMap _) = summary:commands where
    cmds = M.elems cmdMap
    names = map getAppName cmds
    descrs = map (fromMaybe "" . getAppDescr) cmds
    summaryUsage = const $ "{" ++ L.intercalate "," names ++ "}"
    summary = ParamDescr
      summaryUsage "commands arguments" summaryUsage "" ""
    singleCmdDescr name descr = ParamDescr
      (const "") "commands arguments" (const name) descr ""
    commands = zipWith singleCmdDescr names descrs


