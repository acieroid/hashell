module Errors where

import Control.Monad.Error

-- An error
data ShellError = GeneralError String 
                | ReturnCode String Int
                | DirectoryNotFound String

type ThrowsError = Either ShellError

instance Error ShellError where
  noMsg = GeneralError "An error has occured"
  strMsg s = GeneralError s

instance Show ShellError where
  show e = showError e 

-- show an error
showError :: ShellError -> String
showError (GeneralError s) = s
showError (DirectoryNotFound d) = d ++ ": directory not found"
showError (ReturnCode cmd n) = case n of
  1 -> cmd ++ " returned 1"
  2 -> cmd ++ ": error"
  126 -> "can't execute " ++ cmd
  127 -> cmd ++ ": command not found"
  n -> if (n - 128 > 0) then (cmd ++ " got signal " ++ (show $ n-128))
                        else (cmd ++ " returned " ++ (show n) ++
                              " : unknown return value")

