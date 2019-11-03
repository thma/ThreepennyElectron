module Main where

import           System.Environment (getArgs)
import qualified Ui                 (start)

-- | main entry point, also used by the main.js launch script
main :: IO ()
main = do
  [port] <- getArgs
  Ui.start (read port)