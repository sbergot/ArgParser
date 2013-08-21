module System.Console.EasyConsole where

import System.Console.EasyConsole.ArgsProcess

data Parser a = Parser (Args -> a)

data ArgSrc = Pos | Flag
data ArgNbr = No | One | Many

data ArgParserInfo = ArgParserInfo {
  argdescr :: String,
  argkey :: String,
  argsrc :: ArgSrc,
  argnbr :: ArgNbr
  }

data CmdLineAppInfo = CmdLineAppInfo {
  cmdargsdescr :: [ArgParserInfo],
  appname :: String,
  appdescr :: Maybe String
  }
  
data CmdLineParser a = CmdLineParser {
  info :: CmdLineAppInfo,
  parser :: Parser a
  }

maxkeywidth :: Int
maxkeywidth = 60

keyindentwidth :: Int
keyindentwidth = 20

keyindent :: String
keyindent = replicate keyindentwidth ' '

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
simplePosArgParserInfo key = ArgParserInfo "" key Pos One 

