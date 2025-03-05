{-# Language NamedFieldPuns #-}
module Main where

import Prelude hiding (log)

import Data.Text.IO qualified as T
import Data.Validation
import System.Directory (withCurrentDirectory)
import System.Nix.DerivedPath

import Sandstone.GhcMakefile.Parse
import Sandstone.NixCLI
import Sandstone.WriteDerivation

localStoreArgs :: [String]
localStoreArgs =
  [ "--store", storePath
  , "--extra-experimental-features", "nix-command ca-derivations"
  , "--substituters", "http://cache.nixos.org"
  , "--builders", ""
  ]

main :: IO ()
main = do
  _ghcStorePath <- setupDemoStore localStoreArgs

  Right ghcDrvPath <- nixIntantiateInDepNixpkgs localStoreArgs "ghc"
  Right bashDrvPath <- nixIntantiateInDepNixpkgs localStoreArgs "bash"
  Right coreutilsDrvPath <- nixIntantiateInDepNixpkgs localStoreArgs "coreutils"
  Right lndirDrvPath <- nixIntantiateInDepNixpkgs localStoreArgs "xorg.lndir"

  putStrLn "done with eval"

  withCurrentDirectory "./example" $
    nixRunInDepNixpkgs localStoreArgs "ghc" ["-M", "Main.hs"]

  makefile <- T.readFile "example/Makefile"
  print makefile

  (graph, lookupVertex) <- case parseMakefile makefile of
    Failure e -> fail $ show e
    Success a -> pure a

  let ops = StoreOperations
       { insertDerivation = nixDerivationAdd localStoreArgs
       , insertFileFromPath = nixStoreAdd localStoreArgs
       }

  let ctx = PathCtx
       { ghcDrvPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque ghcDrvPath) out
       , bashDrvPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque bashDrvPath) out
       , coreutilsDrvPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque coreutilsDrvPath) out
       , lndirDrvPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque lndirDrvPath) out
       }

  finalDrv <- writeBothDerivations T.putStrLn storeDir ops ctx graph lookupVertex

  nixStoreRealise localStoreArgs finalDrv
