module System.Console.EasyConsole.Run (
  runApp, mkApp
  ) where

import           Data.Maybe
import           System.Console.EasyConsole.ArgsProcess
import           System.Console.EasyConsole.BaseType
import           System.Console.EasyConsole.Format
import           System.Console.EasyConsole.Params
import           System.Console.EasyConsole.Parser
import           System.Environment

runParser :: Parser a -> NiceArgs -> ParseResult a
runParser (Parser parse) args = res where
  (res, _) = parse args

runApp :: CmdLineApp a -> (a -> IO ()) -> IO ()
runApp appspec appfun = do
  args <- getArgs
  let parser = parserfun $ cmdargparser appspec
      niceargs = preprocess args
      normalprocess = case runParser parser niceargs of
        Left errmsg -> putStrLn errmsg
        Right val -> appfun val
      specialprocess = runSpecialFlags appspec niceargs
  fromMaybe normalprocess specialprocess

runSpecialFlags :: CmdLineApp a -> NiceArgs -> Maybe (IO ())
runSpecialFlags app args = loop $ specialFlags app where
  loop [] = Nothing
  loop ((parse, action):rest) =
    case runParser (parserfun parse) args of
    Right True -> Just $ action app
    _ -> loop rest where

defaultSpecialFlags :: SpecialFlags a
defaultSpecialFlags = [
  (liftParam $ FlagParam "help" id,
    putStrLn . showCmdLineAppUsage defaultFormat),
  (liftParam $ FlagParam "version" id,
    putStrLn . showCmdLineVersion)
  ]

mkApp :: ParserSpec a -> IO (CmdLineApp a)
mkApp spec = do
  progname <- getProgName
  return $ CmdLineApp
    spec defaultSpecialFlags progname Nothing Nothing
