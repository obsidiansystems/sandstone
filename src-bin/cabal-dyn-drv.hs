{-# Language NamedFieldPuns #-}
module Main where

import Prelude hiding (log)

import Control.Monad ((<=<), unless)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Validation
import System.Directory (doesDirectoryExist, withCurrentDirectory)
import System.Environment
import System.Nix.StorePath
import System.Nix.DerivedPath
import System.Process

import Sandstone.Cabal
import Sandstone.GhcMakefile.Graph
import Sandstone.GhcMakefile.Parse
import Sandstone.RemoteStore
import Sandstone.WriteDerivation (StoreOperations(..), out, bad)

import Data.Graph (vertices)
import Data.List.NonEmpty qualified as NEL

main :: IO ()
main = do
  let getPathFromEnv = (either (fail . show) pure . parsePathFromText storeDir . T.pack) <=< getEnv
  let getDerivedPathFromEnv = (either (fail . show) pure . parseSingleDerivedPath storeDir . T.pack) <=< getEnv

  ghcPath' <- getPathFromEnv "ghc"

  bashPath' <- getDerivedPathFromEnv "bash"
  coreutilsPath' <- getDerivedPathFromEnv "coreutils"
  lndirPath' <- getDerivedPathFromEnv "lndir"

  sources <- getEnv "sources"
  subdir <- T.pack <$> getEnv "intermediatesSubdir"
  planConfigureFlags <- words <$> getEnv "planConfigureFlags"

  outerName <- T.pack <$> getEnv "name"
  assembleName <- case T.stripSuffix ".drv" outerName of
    Nothing -> fail $ "derivation name should end in .drv, since its output is a derivation: " <> show outerName
    Just n -> pure $ bad n

  buildTop <- getEnv "NIX_BUILD_TOP"
  let work = buildTop <> "/work"
  let ghcBinDir = T.unpack (storePathToText storeDir ghcPath') <> "/bin"

  callProcess "cp" ["-r", "--no-preserve=mode", sources, work]

  let shim = buildTop <> "/ghc-shim"
  writeFile shim $ unlines
    [ "#!/bin/sh"
    , "for a in \"$@\"; do"
    , "  if [ \"$a\" = --make ]; then"
    , "    for b in \"$@\"; do printf '%s\\0' \"$b\"; done > ghc-args.bin"
    , "    exit 0"
    , "  fi"
    , "done"
    , "exec " <> ghcBinDir <> "/ghc \"$@\""
    ]
  callProcess "chmod" ["+x", shim]

  (graph, lookupVertex, cellFlags) <- withCurrentDirectory work $ do
    callProcess (ghcBinDir <> "/ghc") ["--make", "-o", "Setup", "Setup.hs"]
    callProcess "./Setup" $
      [ "configure"
      , "--with-ghc=" <> shim
      , "--with-ghc-pkg=" <> ghcBinDir <> "/ghc-pkg"
      ] <> planConfigureFlags

    -- Aborted on purpose once the shim has captured ghc's argv.
    (_, _, _, ph) <- createProcess (proc "./Setup" ["build"])
    _ <- waitForProcess ph

    argv <- map T.decodeUtf8 . filter (not . BS.null) . BS.split 0 <$> BS.readFile "ghc-args.bin"
    let flags = List.delete "--make" argv

    hasDb <- doesDirectoryExist "dist/package.conf.inplace"
    unless hasDb $
      callProcess (ghcBinDir <> "/ghc-pkg") ["init", "dist/package.conf.inplace"]

    -- Cabal's -odir would prefix the makefile's object paths with
    -- dist/build, which the parser reads as module names.
    callProcess (ghcBinDir <> "/ghc") $ ["-M", "-dep-makefile", "Makefile.sandstone"] <> map T.unpack (dropOutputDirFlags flags)

    makefile <- T.readFile "Makefile.sandstone"
    (graph, lookupVertex) <- case parseMakefile makefile of
      Failure e -> fail $ show e
      Success a -> pure a

    let moduleNames = dotted . (\(a, _, _) -> a) . lookupVertex <$> vertices graph
    pure (graph, lookupVertex, filter (`notElem` moduleNames) flags)

  socketPath <- builderSocketPath
  res <- runBuilderRpc socketPath $ do
    Right autogenPath' <- insertFileFromPath remoteStoreOps (work <> "/dist/build/autogen") "autogen"

    let ctx = CabalCtx
         { ghcPath = ghcPath'
         , autogenPath = autogenPath'
         , bashPath = bashPath'
         , coreutilsPath = coreutilsPath'
         , lndirPath = lndirPath'
         , ghcFlags = cellFlags
         }

    finalDrv <- writeCabalDerivations (liftIO . T.putStrLn) storeDir remoteStoreOps ctx assembleName subdir work graph lookupVertex
    registerOutput finalDrv out
    pure finalDrv

  finalDrv <- either (fail . show) pure res
  T.putStrLn $ "submitted " <> storePathToText storeDir finalDrv <> " as output 'out'"

dotted :: Module -> Text
dotted m = T.intercalate "." $ NEL.toList $ moduleName m

dropOutputDirFlags :: [Text] -> [Text]
dropOutputDirFlags = go
  where
    go (f : _ : rest) | f `elem` ["-outputdir", "-odir", "-hidir", "-hiedir", "-stubdir"] = go rest
    go (f : rest) = f : go rest
    go [] = []
