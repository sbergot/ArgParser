{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Base types shared by several EasyConsole modules.
-}

module System.Console.ArgParser.BaseType where

import           Control.Applicative     ((<*>), Applicative, pure)
import qualified Data.Map as M (Map)

-- | Simple command line arg
type Arg   = String
-- | List of args provided
type Args  = [Arg]
-- | Flag collection with corresponding args
type Flags = M.Map Arg Args
-- | Structured args to be parsed.
--   Pair of (positionnal arguments, flag arguments)
type NiceArgs = (Args, Flags)
-- | Type representing the result of the parse.
--   Right val in case of success or
--   Left msg if there was an error.
type ParseResult a = Either String a

-- | Data structure describing a parameter
data ParamDescr = ParamDescr
  { argUsageFmt :: String -> String -- ^ Short description of the parameter format
  , argCategory :: String -- ^ Category of parameter (optional/mandatory)
  , argFormat   :: String -> String -- ^ Format of the parameter to provide
  , argDescr    :: String -- ^ Description of the parameter
  , argMetaVar  :: String -- ^ Description of the parameter in the usage
  }

-- | Returns a short description of the input format
--   of a parameter.
argUsage :: ParamDescr -> String
argUsage d = argUsageFmt d $ argMetaVar d

-- | Returns a long description of the input format
--   of a parameter.
getArgFormat :: ParamDescr -> String
getArgFormat d = argFormat d $ argMetaVar d

-- | A parser actual function
data Parser a = Parser (NiceArgs -> (ParseResult a, NiceArgs))

-- | Represent a full parameter spec
data ParserSpec a = ParserSpec
  { getParserParams :: [ParamDescr]
  , getParserFun    :: Parser a
  }

instance Functor ParserSpec where
  fmap f p = p {getParserFun = fmap f $ getParserFun p}

instance Applicative ParserSpec where
  pure val = ParserSpec [] $ pure val
  ParserSpec d1 p1 <*> ParserSpec d2 p2 =
    ParserSpec (d1 ++ d2) (p1 <*> p2)

-- | A special action with more possibilities.
--   The full arg list will be provided,
--   with the command line spec itself.
type SpecialAction a =
  CmdLnInterface a
  -> NiceArgs
  -> ParseResult a

-- | A special parser allowing to
--   perform standard actions.
--   Used for version/help/subparsers.
type SpecialFlag a = (ParserSpec Bool, SpecialAction a)

-- | A command line application, with a parser and a description
data CmdLnInterface a = CmdLnInterface
  { cmdArgParser  :: ParserSpec a    -- ^ The argument parser
  , specialFlags  :: [SpecialFlag a] -- ^ The special flags
  , getAppName    :: String          -- ^ The application name.
                                     --   Usally the binary name.
  , getAppVersion :: Maybe String    -- ^ Optional application version
  , getAppDescr   :: Maybe String    -- ^ Optional description
  }

instance Functor Parser where
  fmap f (Parser g) = Parser (\args ->
    let (res, newargs) = g args
    in (fmap f res, newargs))

instance Applicative Parser where
  pure val = Parser parser where
    parser args = (Right val, args)
  (Parser f) <*> (Parser g) = Parser (\args ->
    let (h, newargs) = f args
        (res, lastargs) = g newargs
    in (h <*> res, lastargs))
