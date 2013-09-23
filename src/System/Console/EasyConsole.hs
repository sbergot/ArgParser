module System.Console.EasyConsole (
    module System.Console.EasyConsole.BaseType
  -- * Creating a parser
  , mkApp
  , mkSubParser
  , runApp
  , parsedBy
  , andBy
  -- * Creating parameters
  -- ** Flag parameters
  , boolFlag
  , reqFlag
  , optFlag
  -- ** Positional parameters
  , reqPos
  , optPos
  ) where
import           System.Console.EasyConsole.BaseType
import           System.Console.EasyConsole.Parser      (andBy, parsedBy)
import           System.Console.EasyConsole.QuickParams
import           System.Console.EasyConsole.Run         (mkApp, runApp)
import           System.Console.EasyConsole.SubParser   (mkSubParser)

-- TODO documentation
-- TODO rename to ArgParser
-- TODO add params shortcuts
-- TODO improve top-level export & exposed modules
