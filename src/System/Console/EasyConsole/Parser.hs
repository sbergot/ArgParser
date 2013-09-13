module System.Console.EasyConsole.Parser (
  ParamSpec (..),
  liftParam,
  more
  ) where

import           Control.Applicative
import           System.Console.EasyConsole.BaseType

class ParamSpec spec where
  getparser :: spec res -> Parser res
  getargformat :: spec res -> String
  getdescr :: spec res -> String
  getdescr _ = ""
  getcategory :: spec res -> String

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
