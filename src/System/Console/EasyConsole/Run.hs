{- |
Module      :  $Header$
Description :  command line app functions
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Functions used to build and run command line applications.
-}

module System.Console.EasyConsole.Run
  ( runApp
  , parseArgs
  , mkApp
  , defaultSpecialFlags
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

-- | Runs a command line application with the
--   user provided arguments. If the parsing succeeds,
--   run the application. Print the returned message otherwise 
runApp
  :: CmdLineApp a -- ^ Command line spec
  -> (a -> IO ()) -- ^ Process to run if the parsing success
  -> IO ()
runApp appspec appfun = do
  args <- getArgs
  case parseArgs (preprocess args) appspec of
    Left msg -> putStrLn msg
    Right val -> appfun val

-- | Parse the arguments with the parser
--   provided to the function.
parseArgs
  :: NiceArgs     -- ^ Arguments to parse
  -> CmdLineApp a -- ^ Command line spec
  -> ParseResult a
parseArgs niceargs appspec = fromMaybe normalprocess specialprocess where
  parser = parserfun $ cmdargparser appspec
  normalprocess = runParser parser niceargs
  specialprocess = runSpecialFlags appspec niceargs
  

runSpecialFlags :: CmdLineApp a -> NiceArgs -> Maybe (ParseResult a)
runSpecialFlags app args = loop $ specialFlags app where
  loop flags = case flags of
    []                   -> Nothing
    (parse, action):rest -> runSpecialAction parse action rest
  runSpecialAction parse action other = case specialParseResult of
    Right True -> Just $ action app args
    _          -> loop other
   where
    specialParseResult = runParser (parserfun parse) args

-- | default version and help special actions
defaultSpecialFlags :: [SpecialFlag a]
defaultSpecialFlags =
  [ (flagparser "help", showParser $ showCmdLineAppUsage defaultFormat)
  , (flagparser "version", showParser showCmdLineVersion)
  ] where
  flagparser key = liftParam $ FlagParam key id
  -- ignore args and show the result
  showParser action = newaction where
    newaction app _  = Left $ action app

-- | Build an application with no version/description
--   and with a name equal to the file name.
mkApp
  :: ParserSpec a
  -> IO (CmdLineApp a)
mkApp spec = liftM (mkDefaultApp spec) getProgName

mkDefaultApp :: ParserSpec a -> String -> CmdLineApp a
mkDefaultApp spec progName = CmdLineApp
    spec defaultSpecialFlags progName Nothing Nothing
