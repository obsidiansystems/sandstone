{-# Language NamedFieldPuns #-}
module Main where

import Prelude hiding (log)

import Data.Text.IO qualified as T
import Data.Validation
import System.Directory (withCurrentDirectory)
import System.Nix.DerivedPath
import System.Nix.JSON ()

import Sandstone.GhcMakefile.Parse
import Sandstone.NixCLI
import Sandstone.WriteDerivation

main :: IO ()
main = do
  _ghcStorePath <- setupDemoStore

  Right ghcDrvPath <- nixIntantiateInDepNixpkgs "ghc"
  Right bashDrvPath <- nixIntantiateInDepNixpkgs "bash"
  Right coreutilsDrvPath <- nixIntantiateInDepNixpkgs "coreutils"
  Right lndirDrvPath <- nixIntantiateInDepNixpkgs "xorg.lndir"

  putStrLn "done with eval"

  withCurrentDirectory "./example" $
    nixRunInDepNixpkgs "ghc" ["-M", "Main.hs"]

  makefile <- T.readFile "example/Makefile"
  print makefile

  (graph, lookupVertex) <- case parseMakefile makefile of
    Failure e -> fail $ show e
    Success a -> pure a

  let ops = StoreOperations
       { insertDerivation = nixDerivationAdd
       , insertFileFromPath = nixStoreAdd
       }

  let ctx = PathCtx
       { ghcDrvPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque ghcDrvPath) out
       , bashDrvPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque bashDrvPath) out
       , coreutilsDrvPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque coreutilsDrvPath) out
       , lndirDrvPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque lndirDrvPath) out
       }

  finalDrv <- writeBothDerivations T.putStrLn storeDir ops ctx graph lookupVertex

  nixStoreRealise finalDrv
