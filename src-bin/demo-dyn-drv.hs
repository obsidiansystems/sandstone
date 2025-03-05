{-# Language NamedFieldPuns #-}
module Main where

import Prelude hiding (log)

import Control.Monad ((<=<))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Validation
import System.Directory (copyFile, withCurrentDirectory)
import System.Nix.StorePath
import System.Nix.DerivedPath
import System.Environment
import System.Process

import Sandstone.GhcMakefile.Parse
import Sandstone.NixCLI
import Sandstone.WriteDerivation

localStoreArgs :: [String]
localStoreArgs = []

main :: IO ()
main = do
  let getPathFromEnv = (either (fail . show) pure . parsePathFromText storeDir . T.pack) <=< getEnv
  let getDerivedPathFromEnv = (either (fail . show) pure . parseSingleDerivedPath storeDir . T.pack) <=< getEnv

  -- Different because we need ot use right away
  ghcPath <- getPathFromEnv "ghc"

  bashPath <- getDerivedPathFromEnv  "bash"
  coreutilsPath <- getDerivedPathFromEnv  "coreutils"
  lndirPath <- getDerivedPathFromEnv "lndir"

  sources <- getEnv "sources"

  buildTop <- getEnv "NIX_BUILD_TOP"

  withCurrentDirectory sources $
    callProcess (storePathToFilePath storeDir ghcPath <> "/bin/ghc")
      [ "-M", "Main.hs"
      , "-dep-makefile", buildTop <> "/Makefile"
      ]

  makefile <- T.readFile "Makefile"
  print makefile

  (graph, lookupVertex) <- case parseMakefile makefile of
    Failure e -> fail $ show e
    Success a -> pure a

  let ops = StoreOperations
       { insertDerivation = nixDerivationAdd localStoreArgs
       , insertFileFromPath = nixStoreAdd localStoreArgs
       }

  let ctx = PathCtx
       { ghcPath = SingleDerivedPath_Opaque ghcPath
       , bashPath = bashPath
       , coreutilsPath = coreutilsPath
       , lndirPath = lndirPath
       }

  finalDrv <- writeBothDerivations T.putStrLn storeDir ops ctx sources graph lookupVertex

  outPath <- getEnv "out"
  copyFile (T.unpack $ storePathToText storeDir finalDrv) outPath
