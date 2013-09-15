module System.Console.EasyConsole.Run (runApp) where

import System.Console.EasyConsole.BaseType
import System.Console.EasyConsole.ArgsProcess
import System.Environment

runParser :: Parser a -> Args -> ParseResult a
runParser (Parser parse) args = res where
  (res, _) = parse $ preprocess args

runApp :: CmdLineApp a -> (a -> IO ()) -> IO ()
runApp appspec appfun = do
  args <- getArgs
  let parser = parserfun $ cmdargparser appspec
  case runParser parser args of
    Left errmsg -> putStrLn errmsg
    Right val -> appfun val
  
  

