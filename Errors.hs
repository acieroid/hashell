module Errors where

import Control.Monad.Error

-- One error
data ShellError = GeneralError String 

type ThrowsError = Either ShellError

instance Error ShellError where
  noMsg = GeneralError "An error has occured"
  strMsg s = GeneralError s

instance Show ShellError where
  show e = showError e 

showError :: ShellError -> String
showError (GeneralError s) = s
