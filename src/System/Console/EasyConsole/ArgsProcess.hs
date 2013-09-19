module System.Console.EasyConsole.ArgsProcess (preprocess) where

import           Data.List
import qualified Data.Map                            as M
import           System.Console.EasyConsole.BaseType

preprocess :: Args -> NiceArgs
preprocess args = (pos, flagArgs) where
  (pos, rest) =  collectPos $ tokenize args
  flagArgs = M.fromList $ unfoldr parseFlag rest

data TokenType = Flag | Pos
data Token = Token TokenType Arg

isPos :: Token -> Bool
isPos (Token tokenType _) = case tokenType of
  Flag -> True
  _    -> False

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
