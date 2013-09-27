{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Functions used to specify a parser for command line arguments.
-}

module System.Console.ArgParser.Parser
  ( ParamSpec (..)
  , liftParam
  , parsedBy
  , andBy
  , subParser
  ) where

import           Control.Applicative
import           System.Console.ArgParser.BaseType

-- | interface allowing to define a basic block of a command line parser
class ParamSpec spec where
  getParser :: spec res -> Parser res
  getParamDescr :: spec res -> [ParamDescr]

-- | Converts any "ParamSpec" to a "ParserSpec"
liftParam :: ParamSpec spec => spec res -> ParserSpec res
liftParam param = ParserSpec
  (getParamDescr param)
  $ getParser param

instance ParamSpec ParserSpec where
  getParser = getParserFun
  getParamDescr = getParserParams

infixl 1 `andBy`
-- | Build a parser from a parser and a ParamSpec
--
-- > MyApp `parsedBy` myparamspec `andBy` myotherparamspec
andBy
  :: ParamSpec spec
  => ParserSpec (a -> b)
  -> spec a
  -> ParserSpec b
andBy parser param = parser <*> liftParam param

infixl 1 `parsedBy`
-- | Build a parser from a type constructor and a ParamSpec
--
-- > MyApp `parsedBy` myparamspec
parsedBy
  :: ParamSpec spec
  => (a -> b)
  -> spec a
  -> ParserSpec b
parsedBy constr firstarg = constr <$> liftParam firstarg

infixl 2 `subParser`
subParser
  :: ParamSpec spec
  => (a -> b)
  -> spec a
  -> ParserSpec b
subParser = parsedBy
