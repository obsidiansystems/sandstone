{-# Language NamedFieldPuns #-}
-- | Write per-module derivations for a Cabal package.
--
-- Each compile cell reproduces Cabal's own working layout under
-- @dist/build@ and runs @ghc -c@ with the exact flags Cabal computed
-- at configure time, so the resulting interfaces satisfy GHC's
-- recompilation check when a later stock @Setup build@ resumes from
-- them. The assemble derivation collects every cell's artifacts into
-- the @dist/build@ tree that nixpkgs' @previousIntermediates@ hook
-- imports.
module Sandstone.Cabal where

import Prelude hiding (log)

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Char (isAlphaNum)
import Data.Graph
import Data.Map (Map)
import Data.Map qualified as Map
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.Nix.DerivedPath
import System.Nix.Derivation
import System.Nix.JSON ()
import System.Nix.Placeholder
import System.Nix.StorePath

import Sandstone.GhcMakefile.Graph
import Sandstone.WriteDerivation
  ( StoreOperations(..)
  , DrvMemo
  , out, object, interface
  , outputsFromList
  , bad
  )

data CabalCtx = CabalCtx
  { ghcPath :: StorePath
  , autogenPath :: StorePath
  , bashPath :: SingleDerivedPath
  , coreutilsPath :: SingleDerivedPath
  , lndirPath :: SingleDerivedPath
  , packageDbPaths :: [StorePath]
  , buildPlatform :: Text
  , ghcFlags :: [Text]
  } deriving (Eq, Ord, Show)

writeCabalDerivations
  :: MonadFail m
  => (Text -> m ())
  -> StoreDir
  -> StoreOperations m
  -> CabalCtx
  -> StorePathName
  -> Text
  -> FilePath
  -> (Module -> FilePath)
  -> Graph
  -> (Vertex -> (Module, b, [Module]))
  -> m StorePath
writeCabalDerivations log storeDir ops ctx assembleName subdir sourceRoot srcPath graph lookupVertex = do
  let moduleOf = (\(a, _, _) -> a) . lookupVertex
  -- GHC demand-loads interfaces beyond the direct imports whenever a
  -- dependency's signatures or unfoldings mention deeper modules, so
  -- every cell gets the transitive closure.
  let todo = [ (moduleOf v, [moduleOf w | w <- reachable graph v, w /= v]) | v <- reverseTopSort graph ]
  memo <- flip execStateT Map.empty $ mapM_ (uncurry $ writeCellDerivation' log storeDir ops ctx sourceRoot srcPath) todo
  writeAssembleDerivation log storeDir ops ctx assembleName subdir memo

writeCellDerivation'
  :: MonadFail m
  => (Text -> m ())
  -> StoreDir
  -> StoreOperations m
  -> CabalCtx
  -> FilePath
  -> (Module -> FilePath)
  -> Module
  -> [Module]
  -> StateT DrvMemo m ()
writeCellDerivation' log storeDir ops ctx sourceRoot srcPath node deps = do
  memo <- get
  drvPath <- lift $ writeCellDerivation log storeDir ops ctx sourceRoot srcPath memo node deps
  modify $ Map.insert node drvPath

writeCellDerivation
  :: forall m
  .  MonadFail m
  => (Text -> m ())
  -> StoreDir
  -> StoreOperations m
  -> CabalCtx
  -> FilePath
  -> (Module -> FilePath)
  -> DrvMemo
  -> Module
  -> [Module]
  -> m StorePath
writeCellDerivation log storeDir ops ctx sourceRoot srcPath memo module' deps = do
    let print' :: Show a => a -> m ()
        print' = log . T.pack . show

    print' module'

    Right source <- insertFileFromPath ops
      (sourceRoot <> "/" <> srcPath module')
      (storeNameify $ T.intercalate "." (NEL.toList $ moduleName module') <> "." <> sourceExt module')

    Just deps' <- pure $ traverse (flip Map.lookup memo) deps
    log "==> DEPS:"
    print' deps'
    log "DEPS <=="

    let ghc = storePathToText storeDir (ghcPath ctx)
    let autogen = storePathToText storeDir (autogenPath ctx)
    let bashPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (bashPath ctx)
    let coreutilsPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (coreutilsPath ctx)
    let lndirPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (lndirPath ctx)

    let relSrc = T.pack (pathNoExt module') <> "." <> sourceExt module'
    let built ext = "dist/build/" <> T.pack (pathNoExt module') <> "." <> ext
    let objOut ext = "$object/" <> T.pack (pathNoExt module') <> "." <> ext
    let ifaceOut ext = "$interface/" <> T.pack (pathNoExt module') <> "." <> ext
    let oExt = objectExt module'
    let hiExt = interfaceExt module'
    let dynamicToo = "-dynamic-too" `elem` ghcFlags ctx

    Right result <- insertDerivation ops $ Derivation
      { name = bad $ storeNameify $ "compile-" <> T.intercalate "." (NEL.toList $ moduleName module')
      , outputs = outputsFromList [object, interface]
      , inputs = foldMap
          derivationInputsFromSingleDerivedPath
          $ SingleDerivedPath_Opaque source
          : SingleDerivedPath_Opaque (ghcPath ctx)
          : SingleDerivedPath_Opaque (autogenPath ctx)
          : bashPath ctx
          : coreutilsPath ctx
          : lndirPath ctx
          : (SingleDerivedPath_Opaque <$> packageDbPaths ctx)
          <> (flip SingleDerivedPath_Built interface . SingleDerivedPath_Opaque <$> deps')
      , platform = buildPlatform ctx
      , builder = bashPlaceholder <> "/bin/bash"
      , args = V.fromList
          [ "-c"
          , T.intercalate ";" $
            [ "set -xeu"
            , "mkdir -p dist/build/autogen"
            ]
            <>
            fmap
              (\d -> T.unwords
                [ lndirPlaceholder <> "/bin/lndir"
                , pathOrPlaceholderFromSingleDerivedPath storeDir
                    $ SingleDerivedPath_Built (SingleDerivedPath_Opaque d) interface
                , "dist/build"
                ])
              deps'
            <>
            [ "cp -r --no-preserve=mode " <> autogen <> "/. dist/build/autogen/"
            , ghc <> "/bin/ghc-pkg init dist/package.conf.inplace"
            , "mkdir -p $(dirname " <> relSrc <> ")"
            , "cp " <> storePathToText storeDir source <> " " <> relSrc
            , T.unwords $ [ghc <> "/bin/ghc", "-c", relSrc] <> ghcFlags ctx
            , "mkdir -p $(dirname " <> objOut oExt <> ") $(dirname " <> ifaceOut hiExt <> ")"
            , "cp " <> built oExt <> " " <> objOut oExt
            , "cp " <> built hiExt <> " " <> ifaceOut hiExt
            ]
            <>
            (if dynamicToo
              then
                [ "cp " <> built ("dyn_" <> oExt) <> " " <> objOut ("dyn_" <> oExt)
                , "cp " <> built ("dyn_" <> hiExt) <> " " <> ifaceOut ("dyn_" <> hiExt)
                ]
              else [])
          ]
      , env = Map.fromList
          [ ("object", renderPlaceholder $ createPlaceholder object)
          , ("interface", renderPlaceholder $ createPlaceholder interface)
          , ("PATH", coreutilsPlaceholder <> "/bin")
          ]
      }
    print' result
    pure result

writeAssembleDerivation
  :: forall m
  .  MonadFail m
  => (Text -> m ())
  -> StoreDir
  -> StoreOperations m
  -> CabalCtx
  -> StorePathName
  -> Text
  -> DrvMemo
  -> m StorePath
writeAssembleDerivation log storeDir ops ctx assembleName subdir memo = do
    let print' :: Show a => a -> m ()
        print' = log . T.pack . show

    let deps' = Map.elems memo
    let bashPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (bashPath ctx)
    let coreutilsPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (coreutilsPath ctx)

    let outputPlaceholder o d = pathOrPlaceholderFromSingleDerivedPath storeDir
          $ SingleDerivedPath_Built (SingleDerivedPath_Opaque d) o

    Right result <- insertDerivation ops $ Derivation
      { name = assembleName
      , outputs = outputsFromList [out]
      , inputs = foldMap
          derivationInputsFromSingleDerivedPath
          $ bashPath ctx
          : coreutilsPath ctx
          : concatMap
              (\d ->
                [ SingleDerivedPath_Built (SingleDerivedPath_Opaque d) object
                , SingleDerivedPath_Built (SingleDerivedPath_Opaque d) interface
                ])
              deps'
      , platform = buildPlatform ctx
      , builder = bashPlaceholder <> "/bin/bash"
      , args = V.fromList
          [ "-c"
          , T.intercalate ";" $
            [ "set -xeu"
            , "mkdir -p $out/" <> subdir <> "/build"
            ]
            <>
            concatMap
              (\d ->
                [ "cp -r --no-preserve=mode " <> outputPlaceholder object d <> "/. $out/" <> subdir <> "/build/"
                , "cp -r --no-preserve=mode " <> outputPlaceholder interface d <> "/. $out/" <> subdir <> "/build/"
                ])
              deps'
          ]
      , env = Map.fromList
          [ ("out", renderPlaceholder $ createPlaceholder out)
          , ("PATH", coreutilsPlaceholder <> "/bin")
          ]
      }
    print' result
    pure result

-- Haskell module names can contain characters that store path names
-- cannot, like apostrophes.
storeNameify :: Text -> Text
storeNameify = T.map $ \c ->
  if isAlphaNum c || c `elem` ("+-._?=" :: String) then c else '-'
