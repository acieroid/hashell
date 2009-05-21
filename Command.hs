module Command where

import System.Cmd
import System.Exit
import System.Directory
import List

-- A cammand, can be builtin or not
data Command = Builtin BuiltinCommand | External String

-- Run a command in a environment
runCommand :: Env -> Command -> [String] -> IO ()
runCommand _ (Builtin cmd) args = (snd cmd) (foldl (++) "" args)
runCommand e (External cmd) args = rawSystem e cmd args >> return ()

{-
type InternalCommand = (String, String -> IO ())

--intenalCommands :: [InternalCommand]
internalCommands = [("cd", cd)]

data Command = Internal (InternalCommand, String) | External String

runCommand :: Command -> IO ()
runCommand (External c) = exec c
runCommand (Internal (cmd, args)) = do 
  (snd cmd) args
  return ()


replaceHome :: String -> IO String
replaceHome s | member '~' s =  do
  home <- getHomeDirectory
  return $ replace "~" home s

parseCommand :: String -> Command
parseCommand cmd | isInternalCommand cmd = Internal (head c, foldr (\x l -> l ++ x) "" $ tail $ split cmd ' ')
                 | otherwise = External cmd
                     where c = filter (\(x,_) -> x == (head $ split cmd ' ')) internalCommands 

isInternalCommand :: String -> Bool
isInternalCommand s = helper' internalCommands
  where helper' [] = False
        helper' ((x,_):xs) = x == (head $ split s ' ') || helper' xs

exec :: String -> IO ()
exec cmd = do
  exitCode <- rawSystem (head c) $ parseArgs (tail c)
  case exitCode of 
    ExitSuccess -> return ()
    ExitFailure 1 -> error ("la commande `" ++ cmd ++ "` a retourné 1")
    ExitFailure 2 -> error ("la commande `" ++ cmd ++ "` a retourné 2")
    ExitFailure 126 -> error ("impossible d'éxécuter la commande `" ++ cmd ++ "`(vérifiez les droits)")
    ExitFailure 127 -> error ("commande `" ++ cmd ++ "` non trouvée")
    ExitFailure 130 -> error ("Control-C")
    ExitFailure x -> error ("code d'erreur " ++ (show x) ++ " inconnu pour la commande `" ++ cmd ++ "`")
  where c = split cmd ' '

parseArgs :: [String] -> [String]
parseArgs [] = []
parseArgs ("":xs) = parseArgs xs
parseArgs (x:xs) = x:(parseArgs xs)

-- Commandes internes 

-- TODO: cd -
cd :: String -> IO ()
cd arg | arg == "" = setCurrentDirectory =<< getHomeDirectory
       | otherwise = do
  b <- doesDirectoryExist arg
  if b 
    then setCurrentDirectory arg
    else error ("Le dossier " ++ arg ++ " n'existe pas")

-- Petites fonctions pratiques 
split :: String -> Char -> [String]
split [] _ = [""]
split (c:cs) d
   | c == d = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs d

member :: Eq a => a -> [a] -> Bool
member _ [] = False
member x (l:ls) = x == l || x `member` ls

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)
-}
