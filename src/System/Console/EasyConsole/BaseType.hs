module System.Console.EasyConsole.BaseType where

import qualified Data.Map as M

type Arg = String
type Args = [Arg]
type Flags = M.Map Arg Args
type NiceArgs = (Args, Flags)
type ParseResult a = Either String a

data ParamDescr = ParamDescr {
  argCategory :: String,
  argFormat :: String,
  argDescr :: String
  }
