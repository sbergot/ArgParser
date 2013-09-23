{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Functions used to specify a parser for command line arguments.
-}

module System.Console.EasyConsole.Parser
  ( ParamSpec (..)
  , liftParam
  , parsedby
  , andby
  ) where

import           Control.Applicative
import           System.Console.EasyConsole.BaseType

-- | interface allowing to define a basic block of a command line parser
class ParamSpec spec where
  getparser :: spec res -> Parser res
  getParamDescr :: spec res -> ParamDescr

-- | Converts any "ParamSpec" to a "ParserSpec" 
liftParam :: ParamSpec spec => spec res -> ParserSpec res
liftParam param = ParserSpec
  [getParamDescr param]
  $ getparser param

infixl 1 `andby`
-- | Build a parser from a parser and a ParamSpec
--
-- > MyApp `parsedby` myparamspec `andby` myotherparamspec
andby
  :: ParamSpec spec
  => ParserSpec (a -> b)
  -> spec a
  -> ParserSpec b
andby parser param = parser <*> liftParam param

infixl 1 `parsedby`
-- | Build a parser from a type constructor and a ParamSpec
--
-- > MyApp `parsedby` myparamspec
parsedby
  :: ParamSpec spec
  => (a -> b)
  -> spec a
  -> ParserSpec b
parsedby constr firstarg = constr <$> liftParam firstarg
