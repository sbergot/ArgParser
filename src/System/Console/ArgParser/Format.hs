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

import           Control.Applicative
import           Data.Char                         (isSpace)
import           Data.List                         (intercalate, unfoldr)
import qualified Data.Map                          as M
import           Data.Maybe
import           System.Console.ArgParser.BaseType

-- | Specification of the help layout
data CmdLineFormat = CmdLineFormat
  { maxKeyWidth    :: Int
  , keyIndentWidth :: Int
  , maxDescrWidth  :: Int
  }

-- | Default specification for the help layout
defaultFormat :: CmdLineFormat
defaultFormat = CmdLineFormat 75 1 80

-- | Prints the application name and version
showCmdLineVersion :: CmdLnInterface a -> String
showCmdLineVersion app =  appName ++ appVersion where
  appName = getAppName app
  appVersion = maybe "" (" " ++) $ getAppVersion app

-- | Prints a long usage such as
--
-- @
--   foo bar [bay]
-- @
showCmdLineAppUsage :: CmdLineFormat -> CmdLnInterface a -> String
showCmdLineAppUsage fmt app = (++ "\n\n") . trim $ intercalate "\n"
  [ showCmdLineVersion app
  , appUsage
  , appDescr
  , appParams
  , appEpilog
  ]
 where
  _reflow = reflow $ maxDescrWidth fmt
  appDescr = fromMaybe "" ((++ "\n") . _reflow 0 <$> getAppDescr app)
  appEpilog = fromMaybe "" (_reflow 0 <$> getAppEpilog app)
  paramdescrs = userDescr ++ specialDescr
  userDescr = getParserParams $ cmdArgParser app
  specialDescr = concatMap (getParserParams . fst) $ specialFlags app
  appParams = formatParamDescrs fmt paramdescrs
  appUsage = "usage : " ++ getAppName app ++ " " ++ usage
  usage = unwords $ filter (not . null) $ map argUsage paramdescrs

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
    indent = maxKeyWidth fmt + keyIndentWidth fmt
    descrtext = reflow (maxDescrWidth fmt) indent $ argDescr descr

reflow :: Int -> Int -> String -> String
reflow width indent text = intercalate ('\n' : replicate indent ' ') _lines where
  -- one space is appended to each line so we drop one char
  _lines = map (drop 1) $ unfoldr takeOneLine $ words text
  takeOneLine :: [String] -> Maybe (String, [String])
  takeOneLine = loop 0 ""
  loop currWidth accum rest = case rest of
    [] -> case accum of
      [] -> Nothing
      _  -> Just (accum, rest)
    word:_words -> let
      newWidth = currWidth + 1 + length word
      in if newWidth > width
        then Just (accum, rest)
        else loop newWidth (accum ++ ' ':word)  _words
