module Prompt where

import IO
import Command


prompt :: String
prompt = "% "

runPrompt :: IO String
runPrompt = (putStr prompt) >> hFlush stdout >> getLine
  

