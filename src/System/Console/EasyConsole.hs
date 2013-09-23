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
  --   the following functions. When providing a conversion function,
  --   you may provide two kind of signatures:
  --
  --   * @String -> a@ means that the parameter expect exactly one arg 
  --
  --   * @[String] -> a@ means that the parameter expect any number of args 

  -- ** Flag parameters
  , boolFlag
  , reqFlag
  , optFlag
  -- ** Positional parameters
  , reqPos
  , optPos
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
