{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Collection of functions which are basically shortcuts
of "System.Console.EasyConsole.Params" versions.
-}

module System.Console.EasyConsole.QuickParams (
  -- * Flag parameters
    boolFlag
  , reqFlag
  , optFlag
  -- * Positional parameters
  , reqPos
  , optPos 
  ) where

import System.Console.EasyConsole.Params

-- | A simple command line flag.
--   The parsing function will return True
--   if the flag is present, if the flag is provided to
--   the command line, and False otherwise.
--   For a key @foo@, the flag can either be @--foo@ or @-f@
boolFlag
  :: Key            -- ^ flag key
  -> FlagParam Bool
boolFlag key = FlagParam key id 

-- | A mandatory positional argument parameter
reqPos
  :: Key         -- ^ Param name
  -> (argf -> a) -- ^ Conversion function
  -> StdArgParam argf a
reqPos = StdArgParam Mandatory Pos

-- | An optional positional argument parameter
optPos
  :: a                  -- ^ Default value
  -> Key                -- ^ Param name
  -> (argf -> a)        -- ^ Conversion function
  -> StdArgParam argf a
optPos val = StdArgParam (Optional val) Pos

-- | A mandatory flag argument parameter
reqFlag
  :: Key         -- ^ Flag name
  -> (argf -> a) -- ^ Conversion function
  -> StdArgParam argf a
reqFlag = StdArgParam Mandatory Flag

-- | An optional flag argument parameter
optFlag
  :: a                  -- ^ Default value
  -> Key                -- ^ Flag name
  -> (argf -> a)        -- ^ Conversion function
  -> StdArgParam argf a
optFlag val = StdArgParam (Optional val) Flag
