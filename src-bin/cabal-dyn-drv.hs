{-# Language NamedFieldPuns #-}
module Main where

import Prelude hiding (log)

import Control.Monad ((<=<), unless)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for)
import Data.Validation
import Distribution.PackageDescription (PackageDescription (dataDir, dataFiles, extraSrcFiles, specVersion), packageDescription)
import Distribution.Simple.Glob (matchDirFileGlob)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Verbosity qualified as Verbosity
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory, withCurrentDirectory)
import System.Environment
import System.FilePath (takeFileName, (</>))
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

  subdir <- T.pack <$> getEnv "intermediatesSubdir"
  platform <- T.pack <$> getEnv "system"

  outerName <- T.pack <$> getEnv "name"
  assembleName <- case T.stripSuffix ".drv" outerName of
    Nothing -> fail $ "derivation name should end in .drv, since its output is a derivation: " <> show outerName
    Just n -> pure $ bad n

  let ghcBinDir = T.unpack (storePathToText storeDir ghcPath') <> "/bin"

  -- With no sources env the tree is already unpacked and configured
  -- around us, with the argv-dumping ghc shim recorded at configure
  -- time. Otherwise prepare all of that ourselves.
  mSources <- lookupEnv "sources"
  work <- case mSources of
    Nothing -> getCurrentDirectory
    Just sources -> do
      ghcShim <- getEnv "ghcShim"
      planConfigureFlags <- words <$> getEnv "planConfigureFlags"
      buildTop <- getEnv "NIX_BUILD_TOP"
      let work = buildTop <> "/work"

      callProcess "cp" ["-r", "--no-preserve=mode", sources, work]

      withCurrentDirectory work $ do
        callProcess (ghcBinDir <> "/ghc") ["--make", "-o", "Setup", "Setup.hs"]
        callProcess "./Setup" $
          [ "configure"
          , "--with-ghc=" <> ghcShim
          , "--with-ghc-pkg=" <> ghcBinDir <> "/ghc-pkg"
          ] <> planConfigureFlags

      pure work

  buildTargetArgs <- maybe [] words <$> lookupEnv "buildTarget"
  buildFlagsArgs <- maybe [] words <$> lookupEnv "buildFlags"

  extraFiles <- collectExtraFiles work

  (graph, lookupVertex, cellFlags, srcMap) <- withCurrentDirectory work $ do
    -- Aborted on purpose once the shim has captured ghc's argv.
    (_, _, _, ph) <- createProcess (proc "./Setup" (["build"] <> buildTargetArgs <> buildFlagsArgs))
    code <- waitForProcess ph

    captured <- doesFileExist "ghc-args.bin"
    unless captured $
      fail $ "Setup build failed before invoking ghc, with " <> show code

    argv <- map T.decodeUtf8 . filter (not . BS.null) . BS.split 0 <$> BS.readFile "ghc-args.bin"
    let flags = dropParallelFlags $ List.delete "--make" argv

    hasDb <- doesDirectoryExist "dist/package.conf.inplace"
    unless hasDb $
      callProcess (ghcBinDir <> "/ghc-pkg") ["init", "dist/package.conf.inplace"]

    callProcess (ghcBinDir <> "/ghc") $ ["-M", "-dep-makefile", "Makefile.sandstone"] <> map T.unpack flags

    rawMakefile <- T.readFile "Makefile.sandstone"
    let (makefile, srcMap) = remapMakefile (odirOf flags) rawMakefile
    (graph, lookupVertex) <- case parseMakefile makefile of
      Failure e -> fail $ show e
      Success a -> pure a

    let moduleNames = dotted . (\(a, _, _) -> a) . lookupVertex <$> vertices graph
    pure (graph, lookupVertex, filter (`notElem` moduleNames) flags, srcMap)

  let srcPathOf m = Map.findWithDefault
        (pathNoExt m <> "." <> T.unpack (sourceExt m))
        (pathNoExt m, sourceExt m)
        srcMap

  socketPath <- builderSocketPath
  res <- runBuilderRpc socketPath $ do
    Right autogenPath' <- insertFileFromPath remoteStoreOps (work <> "/dist/build/autogen") "autogen"
    (cellFlags', dbPaths) <- addPackageDbs cellFlags

    extraFilePaths' <- for extraFiles $ \rel -> do
      Right p <- insertFileFromPath remoteStoreOps (work </> rel)
        (storeNameify $ T.pack $ takeFileName rel)
      pure (T.pack rel, p)

    let ctx = CabalCtx
         { ghcPath = ghcPath'
         , autogenPath = autogenPath'
         , bashPath = bashPath'
         , coreutilsPath = coreutilsPath'
         , lndirPath = lndirPath'
         , packageDbPaths = dbPaths
         , extraFilePaths = extraFilePaths'
         , buildPlatform = platform
         , ghcFlags = cellFlags'
         }

    finalDrv <- writeCabalDerivations (liftIO . T.putStrLn) storeDir remoteStoreOps ctx assembleName subdir work srcPathOf graph lookupVertex
    registerOutput finalDrv out
    pure finalDrv

  finalDrv <- either (fail . show) pure res
  T.putStrLn $ "submitted " <> storePathToText storeDir finalDrv <> " as output 'out'"

-- Template Haskell reads files at compile time, and a cell holds one
-- module's source alone, so the package's stated files ride into every
-- cell at their own relative paths.
collectExtraFiles :: FilePath -> IO [FilePath]
collectExtraFiles work = do
  entries <- listDirectory work
  cabalFile <- case filter (".cabal" `List.isSuffixOf`) entries of
    [one] -> pure one
    named -> fail $ "expected one .cabal file in the configured tree, found: " <> show named

  pd <- packageDescription <$> readGenericPackageDescription Verbosity.silent (work </> cabalFile)

  let expand dir = matchDirFileGlob Verbosity.silent (specVersion pd) dir
      dataBase = dataDir pd

  sources <- concat <$> traverse (expand work) (extraSrcFiles pd)
  dat <-
    if null dataBase
    then concat <$> traverse (expand work) (dataFiles pd)
    else map (dataBase </>) . concat <$> traverse (expand (work </> dataBase)) (dataFiles pd)

  pure $ List.nub $ sources <> dat

-- Cabal assembles the dependency package db in a temp dir, which the
-- compile cells cannot see, so it goes into the store with its
-- references scanned.
addPackageDbs :: [Text] -> BuilderRpcM ([Text], [StorePath])
addPackageDbs = go
  where
    storePrefix = T.decodeUtf8 (unStoreDir storeDir) <> "/"
    go ("-package-db" : p : rest)
      | "/" `T.isPrefixOf` p && not (storePrefix `T.isPrefixOf` p) = do
          Right db <- insertFileFromPath remoteStoreOps (T.unpack p) "package.conf.d"
          (rest', dbs) <- go rest
          pure ("-package-db" : storePathToText storeDir db : rest', db : dbs)
    go (f : rest) = do
      (rest', dbs) <- go rest
      pure (f : rest', dbs)
    go [] = pure ([], [])

-- The makefile's paths carry Cabal's -odir prefix on artifacts and
-- real directory prefixes on sources, neither of which are module
-- paths. Sources get rewritten to the module path from their target,
-- with the real location remembered for the cells.
remapMakefile :: Text -> Text -> (Text, Map (FilePath, Text) FilePath)
remapMakefile odirPrefix rawMakefile =
  let (ls, srcMap) = foldr step ([], Map.empty) (T.lines rawMakefile)
  in (T.unlines ls, srcMap)
  where
    step line (ls, m)
      | "#" `T.isPrefixOf` line || T.null line = (line : ls, m)
      | otherwise =
          let (target0, rest) = T.breakOn " : " line
          in case T.stripPrefix " : " rest of
            Nothing -> (line : ls, m)
            Just dep ->
              let target = fromMaybe target0 $ T.stripPrefix odirPrefix target0
                  (modPath, _) = T.breakOn "." target
              in if ".hs" `T.isSuffixOf` dep || ".hs-boot" `T.isSuffixOf` dep
                then
                  let srcExt = if ".hs-boot" `T.isSuffixOf` dep then "hs-boot" else "hs"
                  in ( (target <> " : " <> modPath <> "." <> srcExt) : ls
                     , Map.insert (T.unpack modPath, srcExt) (T.unpack dep) m
                     )
                else
                  ( (target <> " : " <> fromMaybe dep (T.stripPrefix odirPrefix dep)) : ls
                  , m
                  )

dotted :: Module -> Text
dotted m = T.intercalate "." $ NEL.toList $ moduleName m

odirOf :: [Text] -> Text
odirOf ("-odir" : d : _) = d <> "/"
odirOf (_ : rest) = odirOf rest
odirOf [] = "dist/build/"

-- One-shot ghc ignores -j, and RTS options vary with the building
-- machine's core count, which would split the content-addressed cache.
dropParallelFlags :: [Text] -> [Text]
dropParallelFlags = go
  where
    go ("+RTS" : rest) = go $ drop 1 $ dropWhile (/= "-RTS") rest
    go (f : rest) | "-j" `T.isPrefixOf` f && T.all isDigit (T.drop 2 f) = go rest
    go (f : rest) = f : go rest
    go [] = []
