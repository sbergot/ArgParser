{- |
Module      :  $Header$
Description :  base types
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Base types shared by several EasyConsole modules.
-}

module System.Console.EasyConsole.BaseType where

import           Control.Applicative
import qualified Data.Map            as M

-- | Simple command line arg
type Arg   = String
-- | List of args provided
type Args  = [Arg]
-- | Flag collection with corresponding args
type Flags = M.Map Arg Args
-- | Structured args to be parsed.
--   Pair of (positionnal arguments, flag arguments)
type NiceArgs =
  ( Args  
  , Flags
  )
-- | Type representing the result of the parse.
--   Right val in case of success or
--   Left msg if there was an error. 
type ParseResult a = Either String a

-- | Data structure describing a parameter
data ParamDescr = ParamDescr
  { argUsage    :: String -- ^ Short description of the parameter format
  , argCategory :: String -- ^ Category of parameter (optional/mandatory)
  , argFormat   :: String -- ^ Format of the parameter to provide
  , argDescr    :: String -- ^ Description of the parameter
  }

-- | A parser actual function
data Parser a = Parser (NiceArgs -> (ParseResult a, NiceArgs))

-- | Represent a full parameter spec
data ParserSpec a = ParserSpec
  { parserparams :: [ParamDescr]
  , parserfun    :: Parser a
  }

instance Functor ParserSpec where
  fmap f p = p {parserfun = fmap f $ parserfun p}

instance Applicative ParserSpec where
  pure val = ParserSpec [] $ pure val
  ParserSpec d1 p1 <*> ParserSpec d2 p2 =
    ParserSpec (d1 ++ d2) (p1 <*> p2)

-- | A special action with more possibilities.
--   The full arg list will be provided, 
--   with the command line spec itself,
--   and the application process.
type SpecialAction a =
  CmdLineApp a
  -> NiceArgs
  -> ParseResult a

-- | A special parser allowing to
--   perform standard actions.
--   Used for version/help/subparsers. 
type SpecialFlag a = (ParserSpec Bool, SpecialAction a)

-- | A command line application, with a parser and a description
data CmdLineApp a = CmdLineApp
  { cmdargparser :: ParserSpec a    -- ^ The argument parser
  , specialFlags :: [SpecialFlag a] -- ^ The special flags
  , appname      :: String          -- ^ The application name.
                                    --   Usally the binary name.
  , appversion   :: Maybe String    -- ^ Optional application version
  , appdescr     :: Maybe String    -- ^ Optional description
  }

instance Functor Parser where
  fmap f (Parser g) = Parser (\args ->
    let (res, newargs) = g args
    in (fmap f res, newargs))

instance Applicative Parser where
  pure val = Parser parser where
    parser args = (Right val, args)
  (Parser f) <*> (Parser g) = Parser (\args ->
    let (res, newargs) = g args
        (h, lastargs) = f newargs
    in (h <*> res, lastargs))
