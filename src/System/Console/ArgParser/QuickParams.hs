{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Collection of functions which are basically shortcuts
of "System.Console.EasyConsole.Params" versions. If you
cannot find a parameter fitting your needs, you should check this
module.


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

-}

module System.Console.ArgParser.QuickParams (
  -- * Parameters without args
    boolFlag
  -- * Parameters with one arg
  -- ** Flags
  , reqFlag
  , optFlag
  -- ** Positional
  , reqPos
  , optPos
  -- * Parameters with multiple args
  -- ** Flags
  , reqFlagArgs
  , optFlagArgs
  -- ** Positionnal
  , posArgs
  -- ** RawRead class
  , RawRead
  ) where

import           Data.Either                       (partitionEithers)
import           Data.List                       (unfoldr)
import           Control.Applicative
import           System.Console.ArgParser.BaseType
import           System.Console.ArgParser.Params

-- | redefined here for compatibility purpose
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

class RawRead a where
  rawParse :: String -> Maybe (a, String)

-- | A class similar to read. The main difference
--   is that strings are parsed without quotes, and 
--
-- > rawRead "foo" :: Maybe String == Just "foo"
instance RawRead Char where
  rawParse s = case s of
    [] -> Nothing
    c:s' -> Just (c, s')

instance RawRead a => RawRead [a] where
  rawParse s = Just (unfoldr rawParse s, [])

instance RawRead Float where
  rawParse = defaultRawParse

instance RawRead Int where
  rawParse = defaultRawParse

defaultRawParse :: Read t => String -> Maybe (t, String)
defaultRawParse s = (\val -> (val , [])) <$> readMaybe s

rawRead :: RawRead a => String -> Maybe a
rawRead s = fst <$> rawParse s

readArg
  :: RawRead a
  => Key
  -> Arg
  -> ParseResult a
readArg key arg = case rawRead arg of
  Just val -> Right val
  Nothing -> Left $ "Could not parse parameter " ++ key ++ "."
    ++ "Unable to convert " ++ arg


-- | A simple command line flag.
--   The parsing function will return True
--   if the flag is present, if the flag is provided to
--   the command line, and False otherwise.
--   For a key @foo@, the flag can either be @--foo@ or @-f@
boolFlag
  :: Key            -- ^ flag key
  -> FlagParam Bool
boolFlag key = FlagParam Short key id

-- | A mandatory positional argument parameter
reqPos
  :: RawRead a
  => Key         -- ^ Param name
  -> StdArgParam a
reqPos key = StdArgParam Mandatory Pos key (SingleArgParser $ readArg key)

-- | An optional positional argument parameter
optPos
  :: RawRead a
  => a                  -- ^ Default value
  -> Key                -- ^ Param name
  -> StdArgParam a
optPos val key = StdArgParam (Optional val) Pos key (SingleArgParser $ readArg key)

-- | A mandatory flag argument parameter
reqFlag
  :: RawRead a
  => Key         -- ^ Flag name
  -> StdArgParam a
reqFlag key = StdArgParam Mandatory Flag key (SingleArgParser $ readArg key)

-- | An optional flag argument parameter
optFlag
  :: RawRead a
  => a                  -- ^ Default value
  -> Key                -- ^ Flag name
  -> StdArgParam a
optFlag val key = StdArgParam (Optional val) Flag key (SingleArgParser $ readArg key)

readArgs
  :: RawRead a
  => Key
  -> b
  -> (b -> a -> b)
  -> Args
  -> ParseResult b
readArgs key initval accum args = case errors of
  [] -> Right $ foldl accum initval values
  _  -> Left $ unlines errors
 where
  (errors, values) = partitionEithers $ map (readArg key) args

-- | A parameter consuming all the remaining positional parameters
posArgs
  :: RawRead a
  => Key                -- ^ Param name
  -> b                  -- ^ Initial value
  -> (b -> a -> b)      -- ^ Accumulation function
  -> StdArgParam b
posArgs key initval accum = StdArgParam
  Mandatory Pos key (MulipleArgParser $ readArgs key initval accum)

-- | A mandatory flag argument parameter taking multiple arguments
reqFlagArgs
  :: RawRead a
  => Key                -- ^ Flag name
  -> b                  -- ^ Initial value
  -> (b -> a -> b)      -- ^ Accumulation function
  -> StdArgParam b
reqFlagArgs key initval accum = StdArgParam
  Mandatory Flag key (MulipleArgParser $ readArgs key initval accum)

-- | An optional flag argument parameter taking multiple arguments
optFlagArgs
  :: RawRead a
  => b                  -- ^ Default value
  -> Key                -- ^ Flag name
  -> b                  -- ^ Initial value
  -> (b -> a -> b)      -- ^ Accumulation function
  -> StdArgParam b
optFlagArgs val key initval accum = StdArgParam
  (Optional val) Flag key (MulipleArgParser $ readArgs key initval accum)
