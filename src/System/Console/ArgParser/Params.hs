{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Parameters are basic building blocks of a command line parser.
-}

module System.Console.ArgParser.Params (
  -- * Standard constructors
  -- ** Constructor
   StdArgParam (..)
  -- ** Misc types
  , ArgSrc (..)
  , ArgParser (..)
  , Optionality (..)
  , Key
  -- * Special constructors
  , FlagParam (..)
  , Descr (..)
  ) where

import qualified Data.Map                            as M
import           Data.Maybe
import           Data.List
import           System.Console.ArgParser.BaseType
import           System.Console.ArgParser.Parser

-- | identifier used to specify the name of a flag
--   or a positional argument.
type Key = String

deleteMany :: [String] -> Flags -> Flags
deleteMany keys flags = foldl (flip M.delete) flags keys

takeFlag :: String -> Flags -> (Maybe Args, Flags)
takeFlag key flags = (args, rest) where
  args = case mapMaybe lookupflag prefixes of
    [] -> Nothing
    grpargs -> Just $ concat grpargs
  lookupflag _key = M.lookup _key flags
  rest = deleteMany prefixes flags
  prefixes = drop 1 $ inits key

-- | A simple command line flag.
--   The parsing function will be passed True
--   if the flag is present, if the flag is provided to
--   the command line, and False otherwise.
--   For a key @foo@, the flag can either be @--foo@ or @-f@
data FlagParam a =
  FlagParam Key (Bool -> a)

flagformat :: String -> String
flagformat key = "-" ++ first ++ ", --" ++ key where
  first = take 1 key

instance ParamSpec FlagParam where
  getParser (FlagParam key parse) = Parser rawparse where
    rawparse (pos, flags) = case args of
      Just [] -> (Right $ parse True, (pos, rest))
      Just _  -> (Left "unexpected parameter(s)", (pos, rest))
      Nothing -> (Right $ parse False, (pos, rest))
     where
      (args, rest) = takeFlag key flags
  getParamDescr (FlagParam key _) = ParamDescr
    ("[--" ++ key ++ "]")
    "optional arguments"
    (flagformat key)
    ""

infixl 2 `Descr`

-- | Allows the user to provide a description for a particular parameter.
--   Can be used as an infix operator:
--
-- > myparam `Descr` "this is my description"
data Descr spec a = Descr
  { getvalue     :: spec a
  , getuserdescr :: String
  }

instance ParamSpec spec => ParamSpec (Descr spec) where
  getParser = getParser . getvalue
  getParamDescr (Descr inner descr) = 
    (getParamDescr inner) { argDescr = descr }

-- | Defines the source of a parameter: either positional or flag.
data ArgSrc = Flag | Pos

-- | Defines whether a parameter is mandatory or optional.
--   When a parameter is marked as Optional, a default value must
--   be provided.
data Optionality a = Mandatory | Optional a

-- | Defines the number of args consumed by a standard parameter
data ArgParser a =
  -- | Uses exactly one arg
  SingleArgParser (Arg -> ParseResult a) |
  -- | Uses any number of args
  MulipleArgParser (Args -> ParseResult a)

runFlagParse
  :: ArgParser a
  -> Args
  -> ParseResult a
runFlagParse parser args = case parser of
  SingleArgParser f -> case args of
    []    -> Left "missing arg"
    [val] -> f val
    _     -> Left "too many args"
  MulipleArgParser f -> f args

runPosParse
  :: ArgParser a
  -> Args
  -> (ParseResult a, Args)
runPosParse parser args = case parser of
  SingleArgParser f -> case args of
    []       -> (Left "missing arg", [])
    val:rest -> (f val, rest)
  MulipleArgParser f -> (f args, [])

getValFormat :: ArgParser a -> String
getValFormat parser = case parser of
  SingleArgParser _  -> "VAL"
  MulipleArgParser _ -> "[VALS ...]"

-- | Defines a parameter consuming arguments on the command line.
--   The source defines whether the arguments are positional:
--
-- > myprog posarg1 posarg2 ...
--
--   ... or are taken from a flag:
--
-- > myprog --myflag flagarg1 flagarg2 ...
--
--   short form:
--
-- > myprog -m flagarg1 flagarg2 ...
--
--   One can provide two signatures of parsing function using the 'ArgParser type':
--
--   * 'SingleArgParser' means that the parameter expect exactly one arg 
--
--   * 'MulipleArgParser' means that the parameter expect any number of args 
data StdArgParam a =
  StdArgParam (Optionality a) ArgSrc Key (ArgParser a)

instance ParamSpec StdArgParam where
  getParser (StdArgParam opt src key parse) = Parser rawparse where
    rawparse = choosesrc flagparse posparse src

    flagparse (pos, flags) = (logkey key res, (pos, rest)) where
      (margs, rest) = takeFlag key flags
      res = case margs of
        Nothing -> defaultOrError "missing flag"
        Just args -> runFlagParse parse args

    posparse (pos, flags) = case pos of
      [] -> (logkey key $ defaultOrError "missing arg", (pos, flags))
      args -> let (res, rest) = runPosParse parse args
              in  (res, (rest, flags))

    defaultOrError = missing opt

  getParamDescr (StdArgParam opt src key parser) = 
    ParamDescr (wrap opt usage) (category opt) usage ""
   where
    usage = getkeyformat src key ++ "  " ++ getValFormat parser
    wrap Mandatory msg = msg
    wrap _         msg = "[" ++ msg ++ "]"
    

choosesrc :: a -> a -> ArgSrc -> a
choosesrc flag pos src = case src of
  Flag -> flag
  Pos -> pos

getkeyformat :: ArgSrc -> String -> String
getkeyformat = choosesrc id flagformat

missing :: Optionality a -> String -> ParseResult a
missing opt msg = case opt of
  Mandatory    -> Left msg
  Optional val -> Right val

category :: Optionality a -> String
category opt = case opt of
  Mandatory -> "mandatory arguments"
  _         -> "optional arguments"

logkey :: String -> ParseResult a -> ParseResult a
logkey key result = case result of
  Left err -> Left $ "fail to parse '" ++ key ++ "' : " ++ err
  val      -> val
