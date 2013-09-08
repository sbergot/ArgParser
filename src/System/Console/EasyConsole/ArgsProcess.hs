module System.Console.EasyConsole.ArgsProcess (preprocess) where

import           System.Console.EasyConsole.BaseType  
import qualified Data.Map as M
import Data.List

data Token = Flag Arg | Pos Arg

isPos :: Token -> Bool
isPos (Pos _) = True
isPos _ = False

getWord :: Token -> Arg
getWord (Pos word) = word
getWord (Flag word) = word

tokenize :: Args -> [Token]
tokenize = concatMap arg2token where
  arg2token :: Arg -> [Token]
  arg2token ('-':'-':word) = [Flag word] 
  arg2token ('-':word) =  map (Flag . (:[]) ) word 
  arg2token word = [Pos word] 

collectPos :: [Token] -> (Args, [Token])
collectPos tokens = (pos, rest) where
  (posargs, rest) = span isPos tokens
  pos = map getWord posargs

parseFlag :: [Token] -> Maybe ((Arg, Args), [Token])
parseFlag (Flag word : tokens) = Just ((word, args), rest) where
  (args, rest) = collectPos tokens
parseFlag _ = Nothing

preprocess :: Args -> NiceArgs
preprocess args = (pos, flagArgs) where
  (pos, rest) =  collectPos $ tokenize args
  flagArgs = M.fromList $ unfoldr parseFlag rest