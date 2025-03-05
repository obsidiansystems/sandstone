{-# Language NamedFieldPuns #-}
module Sandstone.WriteDerivation where

import Prelude hiding (log)

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Dependent.Sum
import Data.Graph
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Some
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.Nix.ContentAddress
import System.Nix.DerivedPath
import System.Nix.Derivation
import System.Nix.JSON ()
import System.Nix.Hash
import System.Nix.OutputName
import System.Nix.Placeholder
import System.Nix.StorePath

import Sandstone.GhcMakefile.Graph

type DrvMemo = Map Module StorePath

data StoreOperations m = StoreOperations
  { insertDerivation :: Derivation -> m (Either InvalidPathError StorePath)
  , insertFileFromPath :: FilePath -> Text -> m (Either InvalidPathError StorePath)
  }

data PathCtx = PathCtx
  { ghcDrvPath :: SingleDerivedPath
  , bashDrvPath :: SingleDerivedPath
  , coreutilsDrvPath :: SingleDerivedPath
  , lndirDrvPath :: SingleDerivedPath
  } deriving (Eq, Ord, Show)

writeBothDerivations
  :: MonadFail m
  => (Text -> m ())
  -> StoreDir
  -> StoreOperations m
  -> PathCtx
  -> Graph
  -> (Vertex -> (Module, b, [Module]))
  -> m StorePath
writeBothDerivations log storeDir ops ctx graph lookupVertex = do
  let todo = fmap ((\(a, _, b) -> (a, b)) . lookupVertex) $ reverseTopSort graph
  memo <- flip execStateT Map.empty $ mapM_ (uncurry $ writeCompilationDerivation' log storeDir ops ctx) todo
  writeLinkDerivation log storeDir ops ctx memo $ (\(a, _, _) -> a) . lookupVertex <$> vertices graph

writeCompilationDerivation'
  :: MonadFail m
  => (Text -> m ())
  -> StoreDir
  -> StoreOperations m
  -> PathCtx
  -> Module
  -> [Module]
  -> StateT DrvMemo m ()
writeCompilationDerivation' log storeDir ops ctx node deps = do
  memo <- get
  drvPath <- lift $ writeCompilationDerivation log storeDir ops ctx memo node deps
  modify $ Map.insert node drvPath

out, interface, object :: OutputName
out = OutputName $ bad "out"
object = OutputName $ bad "object"
interface = OutputName $ bad "interface"

writeCompilationDerivation
  :: forall m
  .  MonadFail m
  => (Text -> m ())
  -> StoreDir
  -> StoreOperations m
  -> PathCtx
  -> DrvMemo
  -> Module
  -> [Module]
  -> m StorePath
writeCompilationDerivation log storeDir ops ctx memo module' deps = do
    let print' :: Show a => a -> m ()
        print' = log . T.pack . show

    print' module'

    Right source <- insertFileFromPath ops
      ("example/" <> pathNoExt module' <> "." <> T.unpack (sourceExt module'))
      (T.intercalate "." (NEL.toList $ moduleName module') <> "." <> sourceExt module')
    print' source

    log "==> CTX:"
    print' ctx
    log "CTX <=="

    Just deps' <- pure $ traverse (flip Map.lookup memo) deps
    log "==> DEPS:"
    print' deps'
    log "DEPS <=="


    let ghcPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (ghcDrvPath ctx)
    let bashPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (bashDrvPath ctx)
    let coreutilsPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (coreutilsDrvPath ctx)
    let lndirPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (lndirDrvPath ctx)

    Right result2 <- insertDerivation ops $ Derivation
      { name = bad $ "compile-" <> T.intercalate "." (NEL.toList $ moduleName module')
      , outputs = outputsFromList [object, interface]
      , inputs = foldMap
          derivationInputsFromSingleDerivedPath
          $ SingleDerivedPath_Opaque source
          : ghcDrvPath ctx
          : bashDrvPath ctx
          : coreutilsDrvPath ctx
          : lndirDrvPath ctx
          : (flip SingleDerivedPath_Built interface . SingleDerivedPath_Opaque <$> deps')
      , platform = "x86_64-linux"
      , builder = bashPlaceholder <> "/bin/bash"
      , args = let
         objectPath = "$object/" <> T.intercalate "/" (NEL.toList $ moduleName module') <> "." <> objectExt module'
         interfacePath = "$interface/" <> T.intercalate "/" (NEL.toList $ moduleName module') <> "." <> interfaceExt module'
        in V.fromList
          [ "-c"
          , T.intercalate ";" $
            [ "set -xeu"
            , "echo $PATH"
            , "mkdir -p $(dirname " <> objectPath <> ")"
            , "mkdir -p $(dirname " <> interfacePath <> ")"
            ]
            <>
            fmap
              (\d -> T.unwords
                [ lndirPlaceholder <> "/bin/lndir"
                , pathOrPlaceholderFromSingleDerivedPath storeDir
                    $ SingleDerivedPath_Built (SingleDerivedPath_Opaque d) interface
                , "$interface"
                ])
              deps'
            <>
            [ T.unwords $
              [ ghcPlaceholder <> "/bin/ghc"
              , "-c"
              , storePathToText storeDir source
              , "-o", objectPath
              , "-ohi", interfacePath
              , "-hidir $interface"
              ]
            ]
          ]
      , env = Map.fromList
          [ ("object", renderPlaceholder $ createPlaceholder object)
          , ("interface", renderPlaceholder $ createPlaceholder interface)
          , ("PATH", coreutilsPlaceholder <> "/bin")
          ]
      }
    print' result2
    pure result2

writeLinkDerivation
  :: forall m
  .  MonadFail m
  => (Text -> m ())
  -> StoreDir
  -> StoreOperations m
  -> PathCtx
  -> DrvMemo
  -> [Module]
  -> m StorePath
writeLinkDerivation log storeDir ops ctx memo deps = do
    let print' :: Show a => a -> m ()
        print' = log . T.pack . show

    log "==> CTX:"
    print' ctx
    log "CTX <=="

    Just deps' <- pure $ traverse (flip Map.lookup memo) deps
    log "==> DEPS:"
    print' deps'
    log "DEPS <=="


    let ghcPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (ghcDrvPath ctx)
    let bashPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (bashDrvPath ctx)
    let coreutilsPlaceholder = pathOrPlaceholderFromSingleDerivedPath storeDir (coreutilsDrvPath ctx)

    Right result2 <- insertDerivation ops $ Derivation
      { name = bad $ "link"
      , outputs = outputsFromList [out]
      , inputs = foldMap
          derivationInputsFromSingleDerivedPath
          $ ghcDrvPath ctx
          : bashDrvPath ctx
          : coreutilsDrvPath ctx
          : (flip SingleDerivedPath_Built object . SingleDerivedPath_Opaque <$> deps')
      , platform = "x86_64-linux"
      , builder = bashPlaceholder <> "/bin/bash"
      , args = V.fromList
          [ "-c"
          , T.intercalate ";" $
            [ "set -xeu"
            , "echo $PATH"
            , T.unwords $
              [ ghcPlaceholder <> "/bin/ghc"
              , "-o", "$out"
              ]
              <> [ renderDownstreamPlaceholder (downstreamPlaceholderFromSingleDerivedPathBuilt (SingleDerivedPath_Opaque d) object) <> "/" <> T.intercalate "/" (NEL.toList $ moduleName dep) <> "." <> objectExt dep
                 | dep <- deps
                 -- Skip .o-boot files, which are empty
                 , not $ hsBoot dep
                 , Just d <- pure $ Map.lookup dep memo
                 ]
            ]
          ]
      , env = Map.fromList
          [ ("out", renderPlaceholder $ createPlaceholder out)
          , ("PATH", coreutilsPlaceholder <> "/bin")
          ]
      }
    print' result2
    pure result2

caOutputSpec :: ContentAddressedDerivationOutput
caOutputSpec = ContentAddressedDerivationOutput
  { caMethod = ContentAddressMethod_NixArchive
  , caHashAlgo = Some HashAlgo_SHA256
  }

outputsFromList :: [OutputName] -> DSum DerivationType (Map OutputName)
outputsFromList outputs = DerivationType_ContentAddressing :=> (Map.fromList $ (\on -> (on, caOutputSpec)) <$> outputs)

bad :: Text -> StorePathName
bad = either (error . show) id . mkStorePathName
