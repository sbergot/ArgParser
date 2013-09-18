{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module System.Console.EasyConsole.Params
  ( FlagParam (..)
  , Descr (..)
  , StdArgParam (..)
  , ArgSrc (..)
  , Optionality (..)
  ) where

import qualified Data.Map                            as M
import           Data.Maybe
import           Data.List
import           System.Console.EasyConsole.BaseType
import           System.Console.EasyConsole.Parser

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

data FlagParam a = FlagParam String (Bool -> a)

flagformat :: String -> String
flagformat key = "-" ++ first ++ ", --" ++ key where
  first = take 1 key

instance ParamSpec FlagParam where
  getparser (FlagParam key parse) = Parser rawparse where
    rawparse (pos, flags) =
      (Right $ parse found, (pos, rest)) where
        (args, rest) = takeFlag key flags
        found = isJust args
  getParamDescr (FlagParam key _) = ParamDescr
    ("[--" ++ key ++ "]")
    "optional arguments"
    (flagformat key)
    ""

infixl 2 `Descr`

data Descr spec a = Descr
  { getvalue     :: spec a
  , getuserdescr :: String
  }

instance ParamSpec spec => ParamSpec (Descr spec) where
  getparser = getparser . getvalue
  getParamDescr (Descr inner descr) = 
    (getParamDescr inner) { argDescr = descr }

data ArgSrc = Flag | Pos

data Optionality a = Mandatory | Optional a

class ParserArg argformat where
  runflagparse :: (argformat -> res) -> Args -> ParseResult res
  runposparse :: (argformat -> res) -> Args -> (ParseResult res, Args)
  getvalformat :: (argformat -> res) -> String

instance ParserArg Arg where
  runflagparse _        [] = Left "missing arg"
  runflagparse parser [val] = Right $ parser val
  runflagparse _        _     = Left "too many args"

  runposparse _ [] = (Left "missing arg", [])
  runposparse parser (val:rest) = (Right $ parser val, rest)

  getvalformat _ = "VAL"

instance ParserArg Args where
  runflagparse parser vals = Right $ parser vals
  runposparse  parser vals = (Right $ parser vals, [])
  getvalformat _ = "VAL [VALS ...]"

data StdArgParam argformat a =  StdArgParam (Optionality a) ArgSrc String (argformat -> a)

instance ParserArg argformat => ParamSpec (StdArgParam argformat) where
  getparser (StdArgParam opt src key parse) = Parser rawparse where
    rawparse = choosesrc flagparse posparse src

    flagparse (pos, flags) = (logkey key res, (pos, rest)) where
      (margs, rest) = takeFlag key flags
      res = case margs of
        Nothing -> defaultOrError "missing flag"
        Just args -> runflagparse parse args

    posparse (pos, flags) = case pos of
      [] -> (logkey key $ defaultOrError "missing arg", (pos, flags))
      args -> let (res, rest) = runposparse parse args
              in  (res, (rest, flags))

    defaultOrError = missing opt

  getParamDescr (StdArgParam opt src key parser) = 
    ParamDescr (wrap opt usage) (category opt) usage ""
   where
    usage = getkeyformat src key ++ "  " ++ getvalformat parser
    wrap Mandatory msg = msg
    wrap _         msg = "[" ++ msg ++ "]"
    

choosesrc :: a -> a -> ArgSrc -> a
choosesrc flag pos src = case src of
  Flag -> flag
  Pos -> pos

getkeyformat :: ArgSrc -> String -> String
getkeyformat Pos key = key
getkeyformat Flag key = flagformat key

missing :: Optionality a -> String -> ParseResult a
missing Mandatory msg = Left msg
missing (Optional val) _ = Right val

category :: Optionality a -> String
category Mandatory = "mandatory arguments"
category _         = "optional arguments"

logkey :: String -> ParseResult a -> ParseResult a
logkey key (Left err) = Left $ "fail to parse '" ++ key ++ "' : " ++ err
logkey _   val = val
