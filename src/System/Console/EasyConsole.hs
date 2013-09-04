module System.Console.EasyConsole where

import           System.Console.EasyConsole.BaseType
import           System.Console.EasyConsole.Params

data ArgParserInfo = ArgParserInfo {
  argdescr :: String,
  argkey   :: String,
  argsrc   :: ArgSrc
  }

data CmdLineAppInfo = CmdLineAppInfo {
  cmdargsdescr :: [ArgParserInfo],
  appname      :: String,
  appdescr     :: Maybe String
  }

data Parser a = Parser (ParamType a) ArgParserInfo
data CmdLineParser a = CmdLineParser {
  info          :: CmdLineAppInfo,
  cmdlineparser :: Parser a
  }

maxkeywidth :: Int
maxkeywidth = 60

keyindentwidth :: Int
keyindentwidth = 20

keyindent :: String
keyindent = replicate keyindentwidth ' '

-- missing argsrc usage
instance Show ArgParserInfo where
  show arginfo = keyindent ++ formattedkey ++ sep ++ descr
    where
      key = argkey arginfo
      formattedkey = case argsrc arginfo of
        Pos -> key
        Flag -> "-" ++ [head key] ++ ", --" ++ key
      padding = maxkeywidth - length formattedkey
      sep = if padding > 0
        then replicate padding ' '
        else "\n" ++ keyindent ++ replicate maxkeywidth ' '
      descr = argdescr arginfo

simplePosArgParserInfo :: String -> ArgParserInfo
simplePosArgParserInfo key = ArgParserInfo "" key Pos

