module EnvVar where
import Data.IORef

-- Contains all the variables of an environment
type Env = IORef [(String, IORef String)]

-- Create a new environment
newEnv :: IO Env
newEnv = newIORef []

-- Assign or update a variable
setVar :: Env -> String -> String -> IO ()
setVar eRef var value = do
  env <- readIORef eRef
  case (lookup var env) of
    Just v -> writeIORef v value 
    Nothing -> do v <- newIORef value
                  writeIORef eRef ((var,v):env)

-- un-assign a variable
unsetVar :: Env -> String -> IO ()
unsetVar eRef var = do
  env <- readIORef eRef
  case (lookup var env) of 
    Just v -> writeIORef eRef $ 
                (filter (\(x,_) -> x /= var) env)
    Nothing -> return () 
    
-- Get the value of a variable, or "" if not declared
getVar :: Env -> String -> IO String
getVar eRef var = do
  env <- readIORef eRef
  case (lookup var env) of
    Just v -> readIORef v
    Nothing -> return ""
