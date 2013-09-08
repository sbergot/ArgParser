module System.Console.EasyConsole.Params where

import System.Console.EasyConsole.Parser
import qualified Data.Map as M


data Optionality a = Mandatory | Optional a

instance Functor Optionality where
  fmap _ Mandatory = Mandatory
  fmap f (Optional val) = Optional $ f val

data FlagParam a = FlagParam String (Bool -> a)

instance ParamSpec FlagParam where
  getparser (FlagParam key parse) = Parser rawparse where
    rawparse (pos, flags) =
      (Right $ parse $ M.member key flags,
       (pos, M.delete key flags))
  getcategory _ = "optional"
  getargformat (FlagParam key _) = "-" ++ first ++ ", --" ++ key where
    first = take 1 key
    