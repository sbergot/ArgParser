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
    withParseResult
  , runApp
  , parseArgs
  , parseNiceArgs
  -- * Building a parser
  , mkApp
  , mkDefaultApp
  , defaultSpecialFlags
  , setAppDescr
  , setAppEpilog
  , setAppVersion
  , setAppName
  , setDescr
  , setVersion
  , setEpilog
  ) where

import           Control.Monad
import           Data.Maybe
import           System.Console.ArgParser.ArgsProcess
import           System.Console.ArgParser.BaseType
import           System.Console.ArgParser.Format
import           System.Console.ArgParser.Params
import           System.Console.ArgParser.Parser
import           System.Environment

runParser :: Parser a -> NiceArgs -> (Bool, ParseResult a)
runParser (Parser parse) args = let
  (res, rest) = parse args
  in (rest == emptyArgs, res)

-- | Runs a command line application with the
--   user provided arguments. If the parsing succeeds,
--   run the application. Print the returned message otherwise
runApp
  :: CmdLnInterface a -- ^ Command line spec
  -> (a -> IO ()) -- ^ Process to run if the parsing success
  -> IO ()
runApp appspec appfun = do
  args <- getArgs
  either printError appfun $ parseArgs args appspec
    where printError err = do
            putStrLn err
            putStrLn (showCmdLineAppUsage defaultFormat appspec)


-- | Runs an apllication with the user provided arguments.
--   It is a shorter way of calling `mkApp` and `runApp`
withParseResult
  :: ParserSpec a
  -> (a -> IO ())
  -> IO ()
withParseResult parser app = do
  interface <- mkApp parser
  runApp interface app


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
  normalprocess = case runParser parser niceargs of
    (True, res) -> res
    (False, _)  -> Left "too many arguments"
  specialprocess = runSpecialFlags appspec niceargs

runSpecialFlags :: CmdLnInterface a -> NiceArgs -> Maybe (ParseResult a)
runSpecialFlags app args = loop $ specialFlags app where
  loop flags = case flags of
    []                   -> Nothing
    (parse, action):rest -> runSpecialAction parse action rest
  runSpecialAction parse action other = case specialParseResult of
    (_, Right True) -> Just $ action app args
    _          -> loop other
   where
    specialParseResult = runParser (getParserFun parse) args

-- | default version and help special actions
defaultSpecialFlags :: [SpecialFlag a]
defaultSpecialFlags =
  [ ( flagparser Short "help" "show this help message and exit"
    , showParser $ showCmdLineAppUsage defaultFormat
    )
  , ( flagparser Long "version" "print the program version and exit"
    , showParser showCmdLineVersion
    )
  ] where
  flagparser fmt key descr = liftParam $ FlagParam fmt key id `Descr` descr
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
    spec defaultSpecialFlags progName Nothing Nothing Nothing

-- | Set the description of an interface
setAppDescr :: CmdLnInterface a -> String -> CmdLnInterface a
setAppDescr app descr = app {getAppDescr = Just descr }

-- | Set the bottom text of an interface
setAppEpilog :: CmdLnInterface a -> String -> CmdLnInterface a
setAppEpilog app descr = app {getAppEpilog = Just descr }

-- | Set the version of an interface
setAppVersion :: CmdLnInterface a -> String -> CmdLnInterface a
setAppVersion app ver = app {getAppVersion = Just ver }

-- | Set the name of an interface
setAppName :: CmdLnInterface a -> String -> CmdLnInterface a
setAppName app descr = app {getAppName = descr }

-- | Flipped setters (useful to apply using fmap)
setDescr :: String -> CmdLnInterface a -> CmdLnInterface a
setDescr = flip setAppDescr

setEpilog :: String -> CmdLnInterface a -> CmdLnInterface a
setEpilog = flip setAppEpilog

setVersion :: String -> CmdLnInterface a -> CmdLnInterface a
setVersion = flip setAppVersion
