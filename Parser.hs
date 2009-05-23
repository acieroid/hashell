module Parser where
import Prelude hiding (words)
import Text.ParserCombinators.Parsec 
import Command
import Errors

-- TODO: fix the thing with the spaces "ls   ." works, but not "ls  "
line = sepBy word (many1 $ char ' ')
word = many (noneOf " \n") 

parseCmd input = case parse line "unknown" input of
                   Right (cmd:args) -> return $ Right $ command cmd args
                   Left _ -> return $ Left $ ParserError "error" 

debug input = parse line "unknown" input
