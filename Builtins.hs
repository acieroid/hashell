module Builtins where 

import System.Directory
import System.Cmd
import System.Posix.Env

-- A builtincommand, like cd, ...
type BuiltinCommand = (String, String -> IO ())

-- List of the buitins commands
builtinsCommands :: [BuiltinCommand]
builtinsCommands = [("cd", cd)]

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
cd :: String -> IO ()
cd arg | arg == "" = updateOldPwd >> 
                       (setCurrentDirectory =<< getHomeDirectory) >>
                       updatePwd 
       | arg == "-" = do 
                        newPwd <- getVar "OLDPWD" 
                        updateOldPwd 
                        setCurrentDirectory newPwd
                        updatePwd 
       | otherwise = do
                      updateOldPwd 
                      b <- doesDirectoryExist arg
                      if b 
                        then setCurrentDirectory arg >> updatePwd
                        else error ("Le dossier " ++ arg ++ " n'existe pas")
