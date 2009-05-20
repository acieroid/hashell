module Main where

import Prompt
import Command
import Control.OldException
import System
import IO

main = do 
  args <- getArgs
  let prompt = length args == 0
  cmd <- content args
  handle (\e -> putStrLn  $ "hashell: erreur: " ++ 
          (show e)) (runCommand (parseCommand cmd) >> return ())
  if prompt then main else return ()
  where content args | (length args) == 0 = runPrompt
                     | otherwise = openFile (args !! 0) 
                                     ReadMode >>= hGetContents
      
