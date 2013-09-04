module System.Console.EasyConsole.Params where

import           System.Console.EasyConsole.BaseType
import           Data.Maybe                             (isJust)

type ParseResult a = Either String a

data Optionality a = Mandatory | Optional a

data ParamOrd a =
  OneArg  (Arg  -> ParseResult a) |
  ManyArg (Args -> ParseResult a)


data ParamType a =
  WithArgs (ParamOrd a) (Optionality a) ArgSrc |
  FlagParam (Bool -> a)

data ParamSpec a = ParamSpec {
  paramkey    :: String,
  paramsrc    :: ArgSrc,
  paramparser :: ParamType a
  }

checkArg :: ParamType a -> Maybe Args -> ParseResult a
checkArg (WithArgs _ (Optional def) _) Nothing     = Right def
checkArg (WithArgs _ Mandatory _)      Nothing     = Left "missing mandatory argument"
checkArg (WithArgs parser _ _)         (Just args) = runParser parser args
checkArg (FlagParam parser)            args        = Right $ parser $ isJust args

runParser :: ParamOrd a -> Args -> ParseResult a
runParser _                []    = Left "missing  argument(s)"
runParser (OneArg parser)  [arg] = parser arg
runParser (OneArg _ )      _     = Left "too many arguments"
runParser (ManyArg parser) args  = parser args