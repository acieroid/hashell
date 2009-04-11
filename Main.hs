module Main where

import Prompt
import Command
import Control.OldException

main = do 
  cmd <- runPrompt  
  handle (\e -> putStrLn  $ "hashell: erreur: " ++ (show e)) (runCommand (parseCommand cmd) >> return ())
  main
