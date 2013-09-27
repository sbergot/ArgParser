{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Simple command line parsing library.
-}

module System.Console.ArgParser (
  -- * Creating a parser
    mkApp
  , mkSubParser
  , runApp
  , parsedBy
  , andBy
  -- * Creating parameters
  -- | Values provided to 'parsedBy' and 'andBy' should be created with
  --   the following functions.
  -- ** Parameters without args
  , boolFlag
  -- ** Parameters with one arg
  -- *** Flags
  , reqFlag
  , optFlag
  -- *** Positional
  , reqPos
  , optPos 
  -- ** Parameters with multiple args
  -- *** Flags
  , reqFlagArgs
  , optFlagArgs
  -- *** Positionnal
  , posArgs
  -- * Base types
  , module System.Console.ArgParser.BaseType
  ) where
import           System.Console.ArgParser.BaseType
import           System.Console.ArgParser.Parser      (andBy, parsedBy)
import           System.Console.ArgParser.QuickParams
import           System.Console.ArgParser.Run         (mkApp, runApp)
import           System.Console.ArgParser.SubParser   (mkSubParser)

-- TODO documentation of the top level module
-- TODO improve QuickParams doc with failure/success cases
-- TODO add Format tests
-- TODO add Meta Var usage
-- TODO run coverage analysis
