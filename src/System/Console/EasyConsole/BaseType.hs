
module System.Console.EasyConsole.BaseType where

import           Control.Applicative
import qualified Data.Map            as M

type Arg   = String
type Args  = [Arg]
type Flags = M.Map Arg Args
type NiceArgs = (Args, Flags)
type ParseResult a = Either String a

data ParamDescr = ParamDescr
  { argUsage    :: String
  , argCategory :: String
  , argFormat   :: String
  , argDescr    :: String
  }

data Parser a = Parser (NiceArgs -> (ParseResult a, NiceArgs))

data ParserSpec a = ParserSpec
  { parserparams :: [ParamDescr]
  , parserfun    :: Parser a
  }

instance Functor ParserSpec where
  fmap f p = p {parserfun = fmap f $ parserfun p}

instance Applicative ParserSpec where
  pure val = ParserSpec [] $ pure val
  ParserSpec d1 p1 <*> ParserSpec d2 p2 =
    ParserSpec (d1 ++ d2) (p1 <*> p2)

type SpecialFlags a = [(ParserSpec Bool, CmdLineApp a -> IO ())]

data CmdLineApp a = CmdLineApp
  { cmdargparser :: ParserSpec a
  , specialFlags :: SpecialFlags a
  , appname      :: String
  , appversion   :: Maybe String
  , appdescr     :: Maybe String
  }

instance Functor Parser where
  fmap f (Parser g) = Parser (\args ->
    let (res, newargs) = g args
    in (fmap f res, newargs))

instance Applicative Parser where
  pure val = Parser parser where
    parser args = (Right val, args)
  (Parser f) <*> (Parser g) = Parser (\args ->
    let (res, newargs) = g args
        (h, lastargs) = f newargs
    in (h <*> res, lastargs))