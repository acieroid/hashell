module Main where

import Prompt
import Parser
import Command
import System
import IO
import Control.Monad.Error

main = do 
  args <- getArgs
  let prompt = length args == 0
  cmd <- content args
  x <- parseCmd cmd 
  case x of 
    Right c -> do y <- runShellExpr c
                  case y of
                    Right _ -> return ()
                    Left e -> putStrLn $ show e
    Left e -> putStrLn $ show e

  if prompt then main else return ()
  where content args | (length args) == 0 = runPrompt
                     | otherwise = openFile (args !! 0) 
                                     ReadMode >>= hGetContents
      
