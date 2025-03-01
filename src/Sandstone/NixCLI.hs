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
import System.Directory (withCurrentDirectory)
import System.Nix.Derivation
import System.Nix.JSON ()
import System.Nix.StorePath
import System.Process hiding (env)

storeDir :: StoreDir
storeDir = def

nixStoreAdd :: FilePath -> Text -> IO (Either InvalidPathError StorePath)
nixStoreAdd fp name = do
  str <- readProcess "nix" (localStoreArgs <> ["store", "add", fp, "--name", T.unpack name]) ""
  pure $ parsePathFromText storeDir $ T.strip $ T.pack str


nixStoreRealise :: StorePath -> IO ()
nixStoreRealise fp =
  callProcess "nix" (localStoreArgs <> ["build", "-L", T.unpack $ storePathToText storeDir fp <> "^*", "-v"])

nixDerivationAdd :: Derivation -> IO (Either InvalidPathError StorePath)
nixDerivationAdd drv = do
  let drvJson = T.decodeUtf8 $ BSL.toStrict $ Aeson.encode drv
  str <- readProcess "nix" (localStoreArgs <> [ "derivation", "add"]) $ T.unpack drvJson
  pure $ parsePathFromText storeDir $ T.strip $ T.pack str

localStoreArgs :: [String]
localStoreArgs =
  [ "--store", storePath
  , "--extra-experimental-features", "nix-command ca-derivations"
  , "--substituters", "http://cache.nixos.org"
  , "--builders", ""
  ]

storePath :: FilePath
storePath = "/tmp/sand"

ghcGenerateMakefile :: StorePath -> IO ()
ghcGenerateMakefile ghcStorePath = do
  let ghcBinPath = T.unpack $ storePathToText storeDir ghcStorePath <> "/bin/ghc"
  callCommand $ ghcBinPath <> " -M *.hs"

setupDemoStore :: IO ()
setupDemoStore = do
  Right ghcStorePath <- nixBuildInDepNixpkgs "ghc"
  nixCopyTo (localStore storePath) ghcStorePath
  withCurrentDirectory "./example" $
    ghcGenerateMakefile ghcStorePath
  pure ()
 where
  localStore = ("local?root=" <>)

nixIntantiateInDepNixpkgs :: String -> IO (Either InvalidPathError StorePath)
nixIntantiateInDepNixpkgs attr = do
  str <- readProcess "nix-instantiate" (localStoreArgs <> ["./dep/nixpkgs", "-A", attr]) ""
  pure $ parsePathFromText storeDir $ T.strip $ T.pack str

nixBuildInDepNixpkgs :: String -> IO (Either InvalidPathError StorePath)
nixBuildInDepNixpkgs attr = do
  str <- readProcess "nix-build" ["./dep/nixpkgs", "-A", attr] ""
  pure $ parsePathFromText storeDir $ T.strip $ T.pack str

nixCopyTo :: FilePath -> StorePath -> IO ()
nixCopyTo store sp = do
  str <- readProcess "nix" ["copy", "--no-check-sigs", "--to", store, T.unpack $ storePathToText storeDir sp] ""
  print str
  pure ()
