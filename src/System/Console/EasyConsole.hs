{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Simple command line parsing library.
-}

module System.Console.EasyConsole (
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
  , module System.Console.EasyConsole.BaseType
  ) where
import           System.Console.EasyConsole.BaseType
import           System.Console.EasyConsole.Parser      (andBy, parsedBy)
import           System.Console.EasyConsole.QuickParams
import           System.Console.EasyConsole.Run         (mkApp, runApp)
import           System.Console.EasyConsole.SubParser   (mkSubParser)

-- TODO documentation of the top level module
-- TODO rename to ArgParser
-- TODO add tests
-- TODO run coverage analysis
-- TODO improve QuickParams doc with failure/success cases
-- TODO remove ParserArg class and use a sum type for multiple param handling 
