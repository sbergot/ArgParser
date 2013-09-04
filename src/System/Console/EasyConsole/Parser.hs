module System.Console.EasyConsole.Parser where

import System.Console.EasyConsole.BaseType
import System.Console.EasyConsole.Params
import System.Console.EasyConsole.ArgsProcess

data ParamSpec a = ParamSpec {
  paramkey    :: String,
  paramsrc    :: ArgSrc,
  paramparser :: ParamType a
  }

getargs :: ParamSpec a -> ArgConsumer
getargs (ParamSpec key Flag _) = takeFlag key
getargs (ParamSpec _   Pos  (WithArgs (OneArg  _) _)) = takePos
getargs (ParamSpec _   Pos  (WithArgs (ManyArg _) _)) = takeAllPos
getargs _ = error "invalid param"

runparse :: ParamSpec a -> NiceArgs -> (ParseResult a, NiceArgs)
runparse param cmdargs = (res, newcmdargs) where
        (args, newcmdargs) = getargs param cmdargs
        res = checkArg (paramparser param) args

