{-# LANGUAGE CPP #-}
module Paths (getStaticDir) where

#if defined(CABAL)
-- | using cabal
import qualified Paths_ThreepennyElectron (getDataDir)

getStaticDir :: IO FilePath
getStaticDir = (</> "static") `liftM` Paths_ThreepennyElectron.getDataDir

#else
-- | using GHCi
getStaticDir :: IO FilePath
getStaticDir = return "static"

#endif