module Main where

  import System.Environment (getArgs)
  import System.IO          (hSetBuffering, stdout, BufferMode( LineBuffering ))
  import Ui                 (start)
  
  main :: IO ()
  main = do
      hSetBuffering stdout LineBuffering
      [port] <- getArgs
      start (read port)