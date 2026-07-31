{-# Language NamedFieldPuns #-}
module Main where

import Prelude hiding (log)

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch, throwIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Validation
import System.Directory (doesFileExist, removeFile, withCurrentDirectory)
import System.Nix.DerivedPath
import System.Nix.StorePath
import System.Process

import Sandstone.GhcMakefile.Parse
import Sandstone.RemoteStore
import Sandstone.WriteDerivation

storePath :: FilePath
storePath = "/tmp/sand"

sources :: FilePath
sources = "example"

storeArgs :: [String]
storeArgs =
  [ "--store", storePath
  , "--extra-experimental-features", "nix-command ca-derivations"
  , "--substituters", "http://cache.nixos.org"
  , "--builders", ""
  ]

-- Evaluating dep/nixpkgs still needs the nix CLI, the daemon protocol
-- has no notion of evaluation.
nixIntantiateInDepNixpkgs :: String -> IO (Either InvalidPathError StorePath)
nixIntantiateInDepNixpkgs attr = do
  str <- readProcess "nix-instantiate" (storeArgs <> ["./dep/nixpkgs", "-A", attr]) ""
  pure $ parsePathFromText storeDir $ T.strip $ T.pack str

nixBuildInDepNixpkgs :: String -> IO (Either InvalidPathError StorePath)
nixBuildInDepNixpkgs attr = do
  str <- readProcess "nix-build" (storeArgs <> ["./dep/nixpkgs", "-A", attr]) ""
  pure $ parsePathFromText storeDir $ T.strip $ T.pack str

nixRunInDepNixpkgs :: String -> [String] -> IO ()
nixRunInDepNixpkgs attr args =
  callProcess "nix" $ storeArgs <> ["run", "--file", "../dep/nixpkgs", attr, "--"] <> args

daemonSocket :: FilePath
daemonSocket = storePath <> "/nix/var/nix/daemon-socket/socket"

withStoreDaemon :: IO a -> IO a
withStoreDaemon act = do
  stale <- doesFileExist daemonSocket
  when stale $ removeFile daemonSocket
  withCreateProcess (proc "nix" ["daemon", "--store", storePath, "--extra-experimental-features", "nix-command ca-derivations"]) $ \_ _ _ _ -> do
    awaitSocket (300 :: Int)
    retrying (10 :: Int) act
  where
    awaitSocket 0 = fail $ "nix daemon did not create " <> daemonSocket
    awaitSocket n = do
      exists <- doesFileExist daemonSocket
      unless exists $ do
        threadDelay 100_000
        awaitSocket (n - 1)
    retrying n act' = act' `catch` \(e :: IOException) ->
      if n > 0
        then threadDelay 200_000 >> retrying (n - 1) act'
        else throwIO e

main :: IO ()
main = do
  Right _ghcStorePath <- nixBuildInDepNixpkgs "ghc"

  Right ghcPath <- nixIntantiateInDepNixpkgs "ghc"
  Right bashPath <- nixIntantiateInDepNixpkgs "bash"
  Right coreutilsPath <- nixIntantiateInDepNixpkgs "coreutils"
  Right lndirPath <- nixIntantiateInDepNixpkgs "xorg.lndir"

  putStrLn "done with eval"

  withCurrentDirectory sources $
    nixRunInDepNixpkgs "ghc" ["-M", "Main.hs"]

  makefile <- T.readFile $ sources <> "/Makefile"
  print makefile

  (graph, lookupVertex) <- case parseMakefile makefile of
    Failure e -> fail $ show e
    Success a -> pure a

  let ctx = PathCtx
       { ghcPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque ghcPath) out
       , bashPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque bashPath) out
       , coreutilsPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque coreutilsPath) out
       , lndirPath = SingleDerivedPath_Built (SingleDerivedPath_Opaque lndirPath) out
       }

  res <- withStoreDaemon $ runStoreRpc daemonSocket $ do
    finalDrv <- writeBothDerivations (liftIO . T.putStrLn) storeDir declaredStoreOps ctx sources graph lookupVertex
    realiseDerivation finalDrv
    pure finalDrv

  finalDrv <- either (fail . show) pure res
  T.putStrLn $ "built " <> storePathToText storeDir finalDrv
