{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Functions used to build and run command line applications.
-}

module System.Console.ArgParser.Run (
  -- * Running a parser
    runApp
  , parseArgs
  , parseNiceArgs
  -- * Building a parser
  , mkApp
  , mkDefaultApp
  , defaultSpecialFlags
  , setAppDescr
  ) where

import           Control.Monad
import           Data.Maybe
import           System.Console.ArgParser.ArgsProcess
import           System.Console.ArgParser.BaseType
import           System.Console.ArgParser.Format
import           System.Console.ArgParser.Params
import           System.Console.ArgParser.Parser
import           System.Environment

runParser :: Parser a -> NiceArgs -> ParseResult a
runParser (Parser parse) args = fst $ parse args

-- | Runs a command line application with the
--   user provided arguments. If the parsing succeeds,
--   run the application. Print the returned message otherwise
runApp
  :: CmdLnInterface a -- ^ Command line spec
  -> (a -> IO ()) -- ^ Process to run if the parsing success
  -> IO ()
runApp appspec appfun = do
  args <- getArgs
  either putStrLn appfun $ parseArgs args appspec

-- | Parse the arguments with the parser
--   provided to the function.
parseArgs
  :: Args     -- ^ Arguments to parse
  -> CmdLnInterface a -- ^ Command line spec
  -> ParseResult a
parseArgs args = parseNiceArgs niceargs
 where
  niceargs = preprocess args

-- | Parse the arguments with the parser
--   provided to the function.
parseNiceArgs
  :: NiceArgs     -- ^ Arguments to parse
  -> CmdLnInterface a -- ^ Command line spec
  -> ParseResult a
parseNiceArgs niceargs appspec = fromMaybe normalprocess specialprocess
 where
  parser = getParserFun $ cmdArgParser appspec
  normalprocess = runParser parser niceargs
  specialprocess = runSpecialFlags appspec niceargs

runSpecialFlags :: CmdLnInterface a -> NiceArgs -> Maybe (ParseResult a)
runSpecialFlags app args = loop $ specialFlags app where
  loop flags = case flags of
    []                   -> Nothing
    (parse, action):rest -> runSpecialAction parse action rest
  runSpecialAction parse action other = case specialParseResult of
    Right True -> Just $ action app args
    _          -> loop other
   where
    specialParseResult = runParser (getParserFun parse) args

-- | default version and help special actions
defaultSpecialFlags :: [SpecialFlag a]
defaultSpecialFlags =
  [ ( flagparser "help" "show this help message and exit"
    , showParser $ showCmdLineAppUsage defaultFormat
    )
  , ( flagparser "version" "print the program version and exit"
    , showParser showCmdLineVersion
    )
  ] where
  flagparser key descr = liftParam $ FlagParam key id `Descr` descr
  -- ignore args and show the result
  showParser action = const . Left . action

-- | Build an application with no version/description
--   and with a name equal to the file name.
mkApp
  :: ParserSpec a
  -> IO (CmdLnInterface a)
mkApp spec = liftM (mkDefaultApp spec) getProgName

-- | Build an application with no version/description
--   and with a name equal to the provided String.
mkDefaultApp :: ParserSpec a -> String -> CmdLnInterface a
mkDefaultApp spec progName = CmdLnInterface
    spec defaultSpecialFlags progName Nothing Nothing

-- | Set the description of an interface
setAppDescr :: CmdLnInterface a -> String -> CmdLnInterface a
setAppDescr app descr = app {getAppDescr = Just descr }