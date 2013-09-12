module System.Console.EasyConsole where

import           System.Console.EasyConsole.BaseType
import           System.Console.EasyConsole.Params
import           System.Console.EasyConsole.Parser

data CmdLineApp a = CmdLineApp {
  cmdargparser :: ParserSpec a,
  appname      :: String,
  appdescr     :: Maybe String
  }

data CmdLineFormat = CmdLineFormat {
  maxkeywidth    :: Int,
  keyindentwidth :: Int
  }

defaultFormat :: CmdLineFormat
defaultFormat = CmdLineFormat 60 20

showargformat :: CmdLineFormat -> ParamDescr -> String
showargformat fmt descr =
  keyindent ++ formattedkey ++ sep ++ descrtext where
    keyindent = replicate (keyindentwidth fmt) ' '
    formattedkey = argFormat descr
    _maxkeywidth = maxkeywidth fmt
    padding = _maxkeywidth - length formattedkey
    sep = if padding > 0
      then replicate padding ' '
      else "\n" ++ keyindent ++ replicate _maxkeywidth ' '
    descrtext = argDescr descr
