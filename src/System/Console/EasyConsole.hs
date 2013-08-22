module System.Console.EasyConsole where

import System.Console.EasyConsole.ArgsProcess


type ParseResult a = Either String a
data Optionality a = Mandatory | Optional a
data ParamSpec a = 
  NoArg   (Bool -> ParseResult a) | 
  OneArg  (Arg  -> ParseResult a) (Optionality a) | 
  ManyArg (Args -> ParseResult a) (Optionality a)

-- only valid for flags?
-- really ugly
-- submit question
runParser :: ParamSpec a -> Maybe Args -> ParseResult a

runParser (NoArg  parser  ) Nothing          = parser False
runParser (NoArg  parser  ) (Just [])        = parser True 
runParser (NoArg  _       ) (Just _ )        = Left "Unexpected argument"

runParser (OneArg _ (Optional def)) Nothing  = Right def
runParser (OneArg _ Mandatory) Nothing       = Left "missing mandatory argument"
runParser (OneArg _ _) (Just [])             = Left "missing  argument"
runParser (OneArg parser _) (Just [arg])     = parser arg
runParser (OneArg _ _) (Just _)              = Left "too many arguments"

runParser (ManyArg _ (Optional def)) Nothing = Right def
runParser (ManyArg _ Mandatory) Nothing      = Left "missing mandatory argument"
runParser (ManyArg _      _) (Just [])       = Left "missing arguments"
runParser (ManyArg parser _) (Just args)     = parser args
  

data ArgSrc = Pos | Flag
data ArgParserInfo = ArgParserInfo {
  argdescr :: String,
  argkey :: String,
  argsrc :: ArgSrc
  }

data CmdLineAppInfo = CmdLineAppInfo {
  cmdargsdescr :: [ArgParserInfo],
  appname :: String,
  appdescr :: Maybe String
  }
  
data Parser a = Parser (Args -> a)
data CmdLineParser a = CmdLineParser {
  info :: CmdLineAppInfo,
  cmdlineparser :: Parser a
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
simplePosArgParserInfo key = ArgParserInfo "" key Pos

