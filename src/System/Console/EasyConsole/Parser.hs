module System.Console.EasyConsole.Parser where

import System.Console.EasyConsole.BaseType
import Control.Applicative

data Parser a = Parser (NiceArgs -> (ParseResult a, NiceArgs))

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

class ParamSpec spec where
  getparser :: spec res -> Parser res
  getargformat :: spec res -> String
  getdescr :: spec res -> String
  getdescr _ = ""
  getcategory :: spec res -> String

data ParserSpec a = ParserSpec {
  parserparams :: [ParamDescr],
  parserfun :: Parser a
  }

getParamDescr :: ParamSpec spec => spec a -> ParamDescr
getParamDescr param = ParamDescr
  (getcategory param)
  (getargformat param)
  (getdescr param)

liftParam :: ParamSpec spec => spec res -> ParserSpec res
liftParam param = ParserSpec
  [getParamDescr param]
  $ getparser param

more :: ParamSpec spec => ParserSpec (a -> b) -> spec a -> ParserSpec b
more parser param = parser <*> liftParam param
  
instance Functor ParserSpec where
        fmap f p = p {parserfun = fmap f $ parserfun p}

instance Applicative ParserSpec where
  pure val = ParserSpec [] $ pure val
  ParserSpec d1 p1 <*> ParserSpec d2 p2 =
    ParserSpec (d1 ++ d2) (p1 <*> p2)