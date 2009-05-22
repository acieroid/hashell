module Command where

import System.Cmd
import System.Exit
import Control.Monad.Error
import Control.Monad.Error
import System.Directory
import System.Cmd
import System.Posix.Env

-- A builtincommand, like cd, ...
type BuiltinCommand = String -> IO (ThrowsError ())

-- List of the buitins commands
builtinsCommands :: [(String, BuiltinCommand)]
builtinsCommands = [("cd", cd)]

-- A cammand, can be builtin or not
data Command = Builtin BuiltinCommand | External String

-- A shell expression
data ShellExpr = Cmd Command [String]

-- An error
data ShellError = GeneralError String 
                | ReturnCode Command Int
type ThrowsError = Either ShellError

instance Error ShellError where
  noMsg = GeneralError "An error has occured"
  strMsg s = GeneralError s

instance Show ShellError where
  show e = showError e 

-- show an error
showError :: ShellError -> String
showError (GeneralError s) = s
showError (ReturnCode (External cmd) n) = case n of
  1 -> cmd ++ " returned 1"
  2 -> cmd ++ ": error"
  126 -> "can't execute " ++ cmd
  127 -> cmd ++ ": command not found"
  n -> if (n - 128 > 0) then (cmd ++ " got signal " ++ (show $ n-128))
                        else (cmd ++ " returned " ++ (show n) ++
                              " : unknown return value")

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
    ExitFailure n -> return $ throwError $ ReturnCode (External cmd) n

-- Run a shell expression
runShellExpr :: ShellExpr -> IO (ThrowsError ())
runShellExpr (Cmd cmd args) = runCommand cmd args

-- Set a variable
setVar :: String -> String -> IO ()
setVar var value = setEnv var value True

-- Get a variable (or "" if the variable doesn't exists)
getVar :: String -> IO String
getVar var = getEnv var >>= 
              (\x -> case x of
                       Just s -> return s
                       Nothing -> return "")

-- update the $OLDPWD variable
updateOldPwd :: IO ()
updateOldPwd = setVar "OLDPWD" =<< getCurrentDirectory

-- update the $PWD variable
updatePwd :: IO ()
updatePwd = setVar "PWD" =<< getCurrentDirectory

-- change directory
cd :: BuiltinCommand
cd arg | arg == "" = updateOldPwd >> 
                       (setCurrentDirectory =<< getHomeDirectory) >>
                       updatePwd >> return (Right ())
       | arg == "-" = do 
                        newPwd <- getVar "OLDPWD" 
                        updateOldPwd 
                        setCurrentDirectory newPwd
                        updatePwd
                        return (Right ())
       | otherwise = do
                      updateOldPwd 
                      b <- doesDirectoryExist arg
                      if b 
                        then setCurrentDirectory arg >> updatePwd >> return (Right ())
                        else return $ throwError $ GeneralError  "hoho" 
