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

  Right ghcPath <- nixIntantiateInDepNixpkgs localStoreArgs "ghc"
  Right bashPath <- nixIntantiateInDepNixpkgs localStoreArgs "bash"
  Right coreutilsPath <- nixIntantiateInDepNixpkgs localStoreArgs "coreutils"
  Right lndirPath <- nixIntantiateInDepNixpkgs localStoreArgs "xorg.lndir"

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
       { ghcPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque ghcPath) out
       , bashPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque bashPath) out
       , coreutilsPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque coreutilsPath) out
       , lndirPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque lndirPath) out
       }

  finalDrv <- writeBothDerivations T.putStrLn storeDir ops ctx "example" graph lookupVertex

  nixStoreRealise localStoreArgs finalDrv
