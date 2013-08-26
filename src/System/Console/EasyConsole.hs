module System.Console.EasyConsole where

import System.Console.EasyConsole.ArgsProcess
import Data.Maybe (isJust)


type ParseResult a = Either String a
data Optionality a = Mandatory | Optional a
data ParamOrd a =
  OneArg  (Arg  -> ParseResult a) | 
  ManyArg (Args -> ParseResult a) 

data ParamSpec a =
  WithArgs (ParamOrd a) (Optionality a) |
  FlagParam (Bool -> a)

checkArg :: ParamSpec a -> Maybe Args -> ParseResult a
checkArg (WithArgs _ (Optional def)) Nothing     = Right def
checkArg (WithArgs _ Mandatory)      Nothing     = Left "missing mandatory argument"
checkArg (WithArgs parser _)         (Just args) = runParser parser args
checkArg (FlagParam parser)          args        = Right $ parser $ isJust args

runParser :: ParamOrd a -> Args -> ParseResult a
runParser _                []    = Left "missing  argument(s)"
runParser (OneArg parser)  [arg] = parser arg
runParser (OneArg _ )      _     = Left "too many arguments"
runParser (ManyArg parser) args  = parser args
  

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
  
data Parser a = Parser (ParamSpec a) ArgParserInfo
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

