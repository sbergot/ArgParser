{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Module containing helpers to print information
about a parser.
-}

module System.Console.ArgParser.Format (
  -- * Print information about the parser
    showCmdLineAppUsage
  , showCmdLineVersion
    -- * Help formatting
  , CmdLineFormat (..)
  , defaultFormat
  ) where

import           Data.Char                         (isSpace)
import           Data.List                         (intercalate)
import qualified Data.Map                          as M
import           Data.Maybe
import           System.Console.ArgParser.BaseType

-- | Specification of the help layout
data CmdLineFormat = CmdLineFormat
  { maxKeyWidth    :: Int
  , keyIndentWidth :: Int
  }

-- | Default specification for the help layout
defaultFormat :: CmdLineFormat
defaultFormat = CmdLineFormat 30 1

-- | Prints the application name and version
showCmdLineVersion :: CmdLineApp a -> String
showCmdLineVersion app =  appName ++ appVersion where
  appName = getAppName app
  appVersion = fromMaybe "" $ getAppVersion app

-- | Prints a long usage such as
--
-- @
--   foo bar [bay]
-- @
showCmdLineAppUsage :: CmdLineFormat -> CmdLineApp a -> String
showCmdLineAppUsage fmt app = intercalate "\n"
  [ showCmdLineVersion app
  , appUsage
  , appDescr
  , appParams
  ]
 where
  appDescr = fromMaybe "" $ getAppDescr app
  paramdescrs = getParserParams $ cmdArgParser app
  appParams = formatParamDescrs fmt paramdescrs
  appUsage = "usage : " ++ getAppName app ++ " " ++ usage
  usage = unwords $ map argUsage paramdescrs

groupByKey :: Ord k => (a -> k) -> [a] -> [(k, [a])]
groupByKey getkey xs = M.toList $ M.fromListWith (flip (++))
  $ map (\x -> (getkey x, [x])) xs

formatParamDescrs :: CmdLineFormat -> [ParamDescr] -> String
formatParamDescrs fmt paramdescrs = unlines $ map showCategory categories where
  categories :: [(String, [ParamDescr])]
  categories = groupByKey argCategory paramdescrs
  showCategory :: (String, [ParamDescr]) -> String
  showCategory (cat, descrs) =
    cat ++ ":\n" ++ formattedargs where
     formattedargs = unlines $ map (showargformat fmt) descrs

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

showargformat :: CmdLineFormat -> ParamDescr -> String
showargformat fmt descr =
  keyindent ++ trim (formattedkey ++ sep ++ descrtext) where
    keyindent = replicate (keyIndentWidth fmt) ' '
    formattedkey = getArgFormat descr
    _maxkeywidth = maxKeyWidth fmt
    padding =  _maxkeywidth - length formattedkey
    sep = if padding > 0
      then replicate padding ' '
      else "\n" ++ keyindent ++ replicate _maxkeywidth ' '
    descrtext = argDescr descr
