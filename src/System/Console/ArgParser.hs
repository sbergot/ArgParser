{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Simple command line parsing library. This library provides
a small combinator dsl to specify a parser for a datatype.
Running the parser will automatically consume and convert
command line arguments. Default special action such as
help/usage are automatically built from the parser specification.

Here is a quick exemple. First, we need a datatype:

@
data MyTest = MyTest Int Int
  deriving (Show) -- we will print the values
@

Then, we define a parser:

@
myTestParser :: ParserSpec MyTest
myTestParser = MyTest
  \`parsedBy\` reqPos \"pos1\"
  \`andBy\` optPos 0 \"pos2\"
@

we proceed to build an interface and run it:

@
main = do
  interface <- mkApp myTestParser
  runApp interface print
@

Building this code will produce a bin
-}
module System.Console.ArgParser (
  -- * Creating a parser
    parsedBy
  , andBy
  , mkApp
  , mkSubParser
  -- * Running a parser
  , runApp
  -- * Creating parameters
  -- | Values provided to 'parsedBy' and 'andBy' should be created with
  --   the following functions. Those are shortcuts based on data types defined in
  --   "System.Console.ArgParser.Params". The types are inferred. argparser will use
  --   'read' to convert the arguments to haskell values, except for strings
  --   which will be passed unmodified.
  --
  --   Flags can be passed in long form (@--foo@) or short form (@-f@)
  --   You may also provide a prefix form such as @--fo@.
  --
  --   Mandatory parameters will fail if the argument is absent or invalid.
  --   Optional parameters only fail if the argument is invalid (ie @foo@ passed
  --   as @Int@)
  --
  --   Note that single arg parameters need exactly one arg, and that multiple args
  --   parameters can have any number of args (0 included).

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
-- TODO add text reflow
