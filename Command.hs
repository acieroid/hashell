module Command where

import System.Cmd
import Control.Monad.Error
import Errors
import Builtins

-- A cammand, can be builtin or not
data Command = Builtin BuiltinCommand | External String

-- Run a command in a environment
runCommand :: Command -> [String] -> IO (ThrowsError ())
runCommand (Builtin cmd) args = do
  res <- cmd (foldl (++) "" args) 
  case res of
    Right _ -> return $ Right ()
    Left e -> return $ throwError e
runCommand (External cmd) args = rawSystem cmd args >> return (Right ())

