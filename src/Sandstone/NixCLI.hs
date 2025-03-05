{-# Language NamedFieldPuns #-}
-- | For now, we "shell out" using the Nix CLI, but for better
-- performance, we should probably instead directly use the daemon
-- protocol as implemented in 'hnix-store-remote'.
--
-- We would have done that right away, but there are some regression on
-- the WIP PR of 'hnix-store' we are currently using.
module Sandstone.NixCLI where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Default
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Nix.Derivation
import System.Nix.JSON ()
import System.Nix.StorePath
import System.Process hiding (env)

storeDir :: StoreDir
storeDir = def

nixStoreAdd :: [String] -> FilePath -> Text -> IO (Either InvalidPathError StorePath)
nixStoreAdd extraArgs fp name = do
  str <- readProcess "nix" (extraArgs <> ["store", "add", fp, "--name", T.unpack name]) ""
  pure $ parsePathFromText storeDir $ T.strip $ T.pack str


nixStoreRealise :: [String] -> StorePath -> IO ()
nixStoreRealise extraArgs fp =
  callProcess "nix" $ extraArgs <> ["build", "-L", T.unpack $ storePathToText storeDir fp <> "^*", "-v"]

nixDerivationAdd :: [String] -> Derivation -> IO (Either InvalidPathError StorePath)
nixDerivationAdd extraArgs drv = do
  let drvJson = T.decodeUtf8 $ BSL.toStrict $ Aeson.encode drv
  str <- readProcess "nix" (extraArgs <> [ "derivation", "add"]) $ T.unpack drvJson
  pure $ parsePathFromText storeDir $ T.strip $ T.pack str

storePath :: FilePath
storePath = "/tmp/sand"

ghcGenerateMakefile :: StorePath -> IO ()
ghcGenerateMakefile ghcStorePath = do
  let ghcBinPath = T.unpack $ storePathToText storeDir ghcStorePath <> "/bin/ghc"
  callCommand $ ghcBinPath <> " -M *.hs"

setupDemoStore :: [String] -> IO StorePath
setupDemoStore extraArgs = do
  Right ghcStorePath <- nixBuildInDepNixpkgs extraArgs "ghc"
  pure ghcStorePath

nixIntantiateInDepNixpkgs :: [String] -> String -> IO (Either InvalidPathError StorePath)
nixIntantiateInDepNixpkgs extraArgs attr = do
  str <- readProcess "nix-instantiate" (extraArgs <> ["./dep/nixpkgs", "-A", attr]) ""
  pure $ parsePathFromText storeDir $ T.strip $ T.pack str

nixBuildInDepNixpkgs :: [String] -> String -> IO (Either InvalidPathError StorePath)
nixBuildInDepNixpkgs extraArgs attr = do
  str <- readProcess "nix-build" (extraArgs <> ["./dep/nixpkgs", "-A", attr]) ""
  pure $ parsePathFromText storeDir $ T.strip $ T.pack str

nixRunInDepNixpkgs :: [String] -> String -> [String] -> IO ()
nixRunInDepNixpkgs extraArgs attr args =
  callProcess "nix" $ extraArgs <> ["run", "--file", "../dep/nixpkgs", attr, "--"] <> args
