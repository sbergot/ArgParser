{- |
Module      :  $Header$
Description :  parser functions
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

class ParamSpec spec where
  getparser :: spec res -> Parser res
  getParamDescr :: spec res -> ParamDescr

liftParam :: ParamSpec spec => spec res -> ParserSpec res
liftParam param = ParserSpec
  [getParamDescr param]
  $ getparser param

infixl 1 `andby`
andby
  :: ParamSpec spec
  => ParserSpec (a -> b)
  -> spec a
  -> ParserSpec b
andby parser param = parser <*> liftParam param

infixl 1 `parsedby`
parsedby
  :: ParamSpec spec
  => (a -> b)
  -> spec a
  -> ParserSpec b
parsedby constr firstarg = constr <$> liftParam firstarg
