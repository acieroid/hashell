module Parser where
import Prelude hiding (words)
import Text.ParserCombinators.Parsec 
import Command


line = sepBy words (char ' ')
words = many (noneOf " \n")

parseCmd input = case parse line "unknown" input of
                   Right (cmd:args) -> Cmd (External cmd) args
                   Left _ -> error "an error has occured"
