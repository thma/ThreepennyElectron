{-# LANGUAGE CPP #-}
module Paths (getStaticDir) where

import Control.Monad
import System.FilePath

#if defined(CABAL)
-- | using cabal
import qualified Paths_deva (getDataDir)

getStaticDir :: IO FilePath
getStaticDir = (</> "static") `liftM` Paths_deva.getDataDir

#else
-- | using GHCi
getStaticDir :: IO FilePath
getStaticDir = return "static"

#endif