module TograUtil where

import Graphics.Rendering.OpenGL

check :: Bool -> IO String -> IO ()
check cond act =
  if not cond
    then do
      r <- act
      error r
    else return ()

checkGlErrors :: IO ()
checkGlErrors = do
  e <- get errors
  check (e == []) (return (concat (map show e)))
