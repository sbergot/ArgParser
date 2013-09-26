{- |
Module      :  $Header$
Copyright   :  (c) Simon Bergot
License     :  BSD3

Maintainer  :  simon.bergot@gmail.com
Stability   :  unstable
Portability :  portable

Preprocess args from a list of words to a pair containing positional args/flag arguments.
-}

module System.Console.ArgParser.ArgsProcess (preprocess) where

import           Data.List
import qualified Data.Map                            as M
import           System.Console.ArgParser.BaseType

-- | Separate positional arguments from flag arguments
preprocess :: Args -> NiceArgs
preprocess args = (pos, flagArgs) where
  (pos, rest) =  collectPos $ tokenize args
  flagArgs = M.fromList $ unfoldr parseFlag rest

data TokenType = Flag | Pos
data Token = Token TokenType Arg


isPos :: Token -> Bool
isPos (Token tokenType _) = case tokenType of
  Pos -> True
  _   -> False

getWord :: Token -> Arg
getWord (Token _ word) = word

tokenize :: Args -> [Token]
tokenize = concatMap arg2token where
  arg2token :: Arg -> [Token]
  arg2token arg = case arg of
    '-':'-':word -> [Token Flag word]
    '-':word     ->  map (Token Flag . (:[]) ) word
    word         -> [Token Pos word]

collectPos :: [Token] -> (Args, [Token])
collectPos tokens = (pos, rest) where
  (posargs, rest) = span isPos tokens
  pos = map getWord posargs

parseFlag :: [Token] -> Maybe ((Arg, Args), [Token])
parseFlag tokens = case tokens of
  Token Flag word : othertokens -> Just ((word, args), rest)
    where (args, rest) = collectPos othertokens
  _ -> Nothing
