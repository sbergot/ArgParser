module System.Console.EasyConsole.Run
  ( runApp
  , mkApp
  ) where

import           Control.Monad
import           Data.Maybe
import           System.Console.EasyConsole.ArgsProcess
import           System.Console.EasyConsole.BaseType
import           System.Console.EasyConsole.Format
import           System.Console.EasyConsole.Params
import           System.Console.EasyConsole.Parser
import           System.Environment

runParser :: Parser a -> NiceArgs -> ParseResult a
runParser (Parser parse) args = fst $ parse args

runApp
  :: CmdLineApp a
  -> (a -> IO ())
  -> IO ()
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
  loop flags = case flags of
    []                   -> Nothing
    (parse, action):rest -> runSpecialAction parse action rest
  runSpecialAction parse action other = case specialParseResult of
    Right True -> Just $ action app
    _          -> loop other
   where
    specialParseResult = runParser (parserfun parse) args

defaultSpecialFlags :: SpecialFlags a
defaultSpecialFlags =
  [ (flagparser "help", putStrLn . showCmdLineAppUsage defaultFormat)
  , (flagparser "version", putStrLn . showCmdLineVersion)
  ] where
  flagparser key = liftParam $ FlagParam key id

mkApp
  :: ParserSpec a
  -> IO (CmdLineApp a)
mkApp spec = liftM (mkDefaultApp spec) getProgName

mkDefaultApp :: ParserSpec a -> String -> CmdLineApp a
mkDefaultApp spec progName = CmdLineApp
    spec defaultSpecialFlags progName Nothing Nothing
