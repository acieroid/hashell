module Parser where
import Prelude hiding (words)
import Text.ParserCombinators.Parsec 
import Command
import Errors


line = sepBy words (char ' ')
words = many (noneOf " \n")

parseCmd input = case parse line "unknown" input of
                   Right (cmd:args) -> return $ Right $ Cmd (External cmd) args
                   Left _ -> return $ Left $ ParserError "error" 
