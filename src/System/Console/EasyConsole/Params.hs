module System.Console.EasyConsole.Params where

import qualified Data.Map                            as M
import           System.Console.EasyConsole.BaseType
import           System.Console.EasyConsole.Parser

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
  getvalue     :: spec a,
  getuserdescr :: String
  }

instance ParamSpec spec => ParamSpec (Descr spec) where
  getparser = getparser . getvalue
  getcategory = getcategory . getvalue
  getargformat = getargformat . getvalue
  getdescr = getuserdescr

data ArgSrc = Flag | Pos

choosesrc :: a -> a -> ArgSrc -> a
choosesrc flag pos src = case src of
  Flag -> flag
  Pos -> pos

argformat :: ArgSrc -> String -> String
argformat Pos key = key ++ "VAL"
argformat Flag key = flagformat key ++ " VAL"

data Optionality a = Mandatory | Optional a

missing :: Optionality a -> String -> ParseResult a
missing Mandatory msg = Left msg
missing (Optional val) _ = Right val

category :: Optionality a -> String
category Mandatory = "mandatory"
category _         = "optional"

logkey :: String -> ParseResult a -> ParseResult a
logkey key (Left err) = Left $ "fail to parse " ++ key ++ " : " ++ err
logkey _   val = val

data SingleArgParam a = SingleArgParam (Optionality a) ArgSrc String (Arg -> a)

instance ParamSpec SingleArgParam where
  getparser (SingleArgParam opt src key parse) = Parser rawparse where
    rawparse = choosesrc flagparse posparse src
    defaultOrError = missing opt

    flagparse (pos, flags) = (logkey key res, (pos, M.delete key flags)) where
      res = case M.lookup key flags of
        Nothing -> defaultOrError "missing flag"
        Just [] -> Left "missing arg"
        Just [val] -> Right $ parse val
        Just _ -> Left "too many args"

    posparse (pos, flags) = case pos of
      [] -> (logkey key $ defaultOrError "missing arg", (pos, flags))
      val:rest -> (Right $ parse val, (rest, flags))

  getcategory (SingleArgParam opt _ _ _) = category opt
  getargformat (SingleArgParam _ src key _) = argformat src key

data MultipleArgParam a = MultipleArgParam (Optionality a) ArgSrc String (Args -> a)

instance ParamSpec MultipleArgParam where
  getparser (MultipleArgParam opt src key parse) = Parser rawparse where
    rawparse = choosesrc flagparse posparse src
    defaultOrError = missing opt

    flagparse (pos, flags) = (logkey key res, (pos, M.delete key flags)) where
      res = case M.lookup key flags of
        Nothing -> defaultOrError "missing flag"
        Just vals -> Right $ parse vals

    posparse (pos, flags) = (Right $ parse pos, ([], flags))

  getcategory (MultipleArgParam opt _ _ _) = category opt
  getargformat (MultipleArgParam _ src key _) = argformat src key
