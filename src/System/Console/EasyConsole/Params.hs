module System.Console.EasyConsole.Params where

import System.Console.EasyConsole.BaseType
import System.Console.EasyConsole.Parser
import qualified Data.Map as M

data FlagParam a = FlagParam String (Bool -> a)

flagformat :: String -> String
flagformat key = "-" ++ first ++ ", --" ++ key where
  first = take 1 key

instance ParamSpec FlagParam where
  getparser (FlagParam key parse) = Parser rawparse where
    rawparse (pos, flags) =
      (Right $ parse $ M.member key flags,
       (pos, M.delete key flags))
  getcategory _ = "optional"
  getargformat (FlagParam key _) = flagformat key

data Descr spec a = Descr {
  getvalue :: spec a,
  getuserdescr :: String
  }

instance ParamSpec spec => ParamSpec (Descr spec) where
  getparser = getparser . getvalue
  getcategory = getcategory . getvalue
  getargformat = getargformat . getvalue
  getdescr = getuserdescr

data ArgSrc = Flag | Pos
data Optionality a = Mandatory | Optional a
data SingleArgParam a = SingleArgParam ArgSrc String (Arg -> a)

-- TODO manage optionality
instance ParamSpec SingleArgParam where
  getparser (SingleArgParam src key parse) = Parser rawparse where
    rawparse = case src of
      Flag -> flagparse
      Pos -> posparse
    flagparse (pos, flags) = (res, (pos, M.delete key flags)) where
      res = case M.lookup key flags of
        Nothing -> Left "missing flag"
        Just [] -> Left "missing arg"
        Just [val] -> Right $ parse val
        Just _ -> Left "too many args" 
    posparse (pos, flags) = case pos of
      [] -> (Left "missing arg", (pos, flags))
      val:rest -> (Right $ parse val, (rest, flags))
  getcategory _ = "optional"
  getargformat (SingleArgParam src key _) = case src of
    Flag -> flagformat key ++ " VAL"
    Pos -> key ++ "VAL"