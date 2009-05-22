module Command where

import Control.Monad.Error
import System.Cmd
import System.Exit

import Errors 
import Builtins

-- A cammand, can be builtin or not
data Command = Builtin BuiltinCommand | External String

-- A shell expression
data ShellExpr = Cmd Command [String]

-- Run a command 
runCommand :: Command -> [String] -> IO (ThrowsError ())
runCommand (Builtin cmd) args = do
  res <- cmd (foldl (++) "" args) 
  case res of
    Right _ -> return $ Right ()
    Left e -> return $ throwError e
runCommand (External cmd) args = do
  exitCode <- rawSystem cmd args 
  case exitCode of
    ExitSuccess -> return $ Right ()
    ExitFailure n -> return $ throwError $ ReturnCode cmd n

-- Run a shell expression
runShellExpr :: ShellExpr -> IO (ThrowsError ())
runShellExpr (Cmd cmd args) = runCommand cmd args

