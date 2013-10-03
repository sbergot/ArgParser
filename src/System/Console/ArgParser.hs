{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Simple command line parsing library. This library provides a small combinator
dsl to specify a parser for a datatype. Running the parser will automatically
consume and convert command line arguments. Default special action such as
help/usage are automatically built from the parser specification.

Here is a quick example. First, we need a datatype:

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

Building this app will produce an executable `foo` which will behave like this:

@
$ foo 1 2
MyTest 1 2
$ foo 3
MyTest 3 0
$ foo -h
foo
usage : foo pos1 [pos2] [-h] [--version]

mandatory arguments:
 pos1

optional arguments:
 pos2
 -h, --help                    show this help message and exit
 --version                     print the program version and exit
@


-}
module System.Console.ArgParser (
  -- * Creating a parser
  -- ** Basics
    parsedBy
  , andBy
  , mkApp
  , mkDefaultApp
  -- ** Adding descriptions
  -- $description
  , Descr (Descr)
  , setAppDescr
  , setAppEpilog
  -- ** Sub commands
  -- $subparser
  , mkSubParser
  -- * Running a parser
  , runApp
  , parseArgs
  -- * Creating parameters
  -- $parameters
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
import           System.Console.ArgParser.Params      (Descr (Descr))
import           System.Console.ArgParser.Parser      (andBy, parsedBy)
import           System.Console.ArgParser.QuickParams
import           System.Console.ArgParser.Run         (mkApp, mkDefaultApp,
                                                       parseArgs, runApp,
                                                       setAppDescr,
                                                       setAppEpilog)
import           System.Console.ArgParser.SubParser   (mkSubParser)


-- TODO copy the first example from the python argparse doc

{- $subparser

You can also split different parsers of the same type into sub-commands with 'mkSubParser':

@
data MyTest =
  MyCons1 Int Int |
  MyCons2 Int
  deriving (Eq, Show)

myTestParser :: IO (CmdLnInterface MyTest)
myTestParser = mkSubParser
  [ (\"A\", mkDefaultApp
    (MyCons1 \`parsedBy\` reqPos \"pos1\" \`andBy\` reqPos \"pos2\") \"A\")
  , (\"B\", mkDefaultApp
    (MyCons2 \`parsedBy\` reqPos \"pos1\") \"B\")
  ]


main = do
  interface <- myTestParser
  runApp interface print
@

Running this script will yield:

@
$ hscmd A 1 2
MyCons1 1 2
$ hscmd B 3
MyCons2 3
$ hscmd -h
hscmd
usage : hscmd {A,B} [-h] [--version]

commands arguments:
 {A,B}
 A
 B

optional arguments:
 -h, --help                    show this help message and exit
 --version                     print the program version and exit

$ hscmd A -h
hscmd A
usage : hscmd A pos1 pos2 [-h] [--version]

mandatory arguments:
 pos1
 pos2

optional arguments:
 -h, --help                    show this help message and exit
 --version                     print the program version and exit
 @

-}

{- $description

You can add descriptions for individual arguments and for the application:

@
import System.Console.ArgParser
import Control.Applicative

data MyTest = MyTest Int Int
  deriving (Show) -- we will print the values

myTestParser :: ParserSpec MyTest
myTestParser = MyTest
  \`parsedBy\` reqPos \"pos1\" \`Descr\` \"description for the first argument\"
  \`andBy\` optPos 0 \"pos2\" \`Descr\` \"description for the second argument\"

myTestInterface :: IO (CmdLnInterface MyTest)
myTestInterface =
  (\`setAppDescr\` \"top description\")
  \<$\> (\`setAppEpilog\` \"bottom description\")
  \<$\> mkApp myTestParser

main = do
  interface <- myTestInterface
  runApp interface print
@

The new help will look like:

@
foo
usage : foo pos1 [pos2] [-h] [--version]
top description

mandatory arguments:
 pos1                          description for the first argument

optional arguments:
 pos2                          description for the second
                               argument
 -h, --help                    show this help message and exit
 --version                     print the program version and exit


bottom description
@

-}

{- $parameters

Values provided to 'parsedBy' and 'andBy' should be created with
the following functions. Those are shortcuts based on data types defined in
"System.Console.ArgParser.Params". The types are inferred. argparser will use
'read' to convert the arguments to haskell values, except for strings
which will be passed unmodified.

Flags can be passed in long form (@--foo@) or short form (@-f@)
You may also provide a prefix form such as @--fo@.

Mandatory parameters will fail if the argument is absent or invalid.
Optional parameters only fail if the argument is invalid (ie @foo@ passed
as @Int@)

Note that single arg parameters need exactly one arg, and that multiple args
parameters can have any number of args (0 included).

Those functions are all defined in "System.Console.ArgParser.QuickParams".

-}
