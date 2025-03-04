{-# Language NamedFieldPuns #-}
module Main where

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
import Data.Text.IO qualified as T
import Data.Validation
import Data.Vector qualified as V
import System.Directory (withCurrentDirectory)
import System.Nix.ContentAddress
import System.Nix.DerivedPath
import System.Nix.Derivation
import System.Nix.JSON ()
import System.Nix.Hash
import System.Nix.OutputName
import System.Nix.Placeholder
import System.Nix.StorePath

import Sandstone.MakefileGraph
import Sandstone.MakefileParse
import Sandstone.NixCLI

main :: IO ()
main = do
  _ghcStorePath <- setupDemoStore

  Right ghcDrvPath <- nixIntantiateInDepNixpkgs "ghc"
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

  let todo = fmap ((\(a, _, b) -> (a, b)) . lookupVertex) $ reverseTopSort graph

  let ops = StoreOperations
       { insertDerivation = nixDerivationAdd
       , insertFileFromPath = nixStoreAdd
       }

  let ctx = PathCtx
       { ghcDrvPath = SingleDerivedPath_Opaque ghcDrvPath
       , coreutilsDrvPath = SingleDerivedPath_Opaque coreutilsDrvPath
       , lndirDrvPath = SingleDerivedPath_Opaque lndirDrvPath
       }

  memo <- flip execStateT Map.empty $ mapM_ (uncurry $ writeDerivation' T.putStrLn ops ctx) todo

  nixStoreRealise $ (memo Map.!) $ (\(a, _, _) -> a) $ lookupVertex $ Prelude.head $ topSort graph

type DrvMemo = Map Node StorePath

data StoreOperations m = StoreOperations
  { insertDerivation :: Derivation -> m (Either InvalidPathError StorePath)
  , insertFileFromPath :: FilePath -> Text -> m (Either InvalidPathError StorePath)
  }

data PathCtx = PathCtx
  { ghcDrvPath :: SingleDerivedPath
  , coreutilsDrvPath :: SingleDerivedPath
  , lndirDrvPath :: SingleDerivedPath
  } deriving (Eq, Ord, Show)

writeDerivation'
  :: MonadFail m
  => (Text -> m ())
  -> StoreOperations m
  -> PathCtx
  -> Node
  -> [Node]
  -> StateT DrvMemo m ()
writeDerivation' log ops ctx node deps = do
  memo <- get
  drvPath <- lift $ writeDerivation log ops ctx memo node deps
  modify $ Map.insert node drvPath

interface, object :: OutputName
object = OutputName $ bad "object"
interface = OutputName $ bad "interface"

writeDerivation
  :: forall m
  .  MonadFail m
  => (Text -> m ())
  -> StoreOperations m
  -> PathCtx
  -> DrvMemo
  -> Node
  -> [Node]
  -> m StorePath
writeDerivation log ops ctx memo node deps = case node of
  Node_Compile module' -> do
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


    let ghcPlaceholder = renderDownstreamPlaceholder $ downstreamPlaceholderFromSingleDerivedPathBuilt (ghcDrvPath ctx) (OutputName $ bad "out")
    let coreutilsPlaceholder = renderDownstreamPlaceholder $ downstreamPlaceholderFromSingleDerivedPathBuilt (coreutilsDrvPath ctx) (OutputName $ bad "out")
    let lndirPlaceholder = renderDownstreamPlaceholder $ downstreamPlaceholderFromSingleDerivedPathBuilt (lndirDrvPath ctx) (OutputName $ bad "out")

    Right result2 <- insertDerivation ops $ Derivation
      { name = bad $ "compile-" <> T.intercalate "." (NEL.toList $ moduleName module')
      , outputs = outputsFromList [object, interface]
      , inputs = foldMap
          derivationInputsFromSingleDerivedPath
          $ SingleDerivedPath_Opaque source
          : SingleDerivedPath_Built (ghcDrvPath ctx) (OutputName $ bad "out")
          : SingleDerivedPath_Built (coreutilsDrvPath ctx) (OutputName $ bad "out")
          : SingleDerivedPath_Built (lndirDrvPath ctx) (OutputName $ bad "out")
          : (flip SingleDerivedPath_Built interface . SingleDerivedPath_Opaque <$> deps')
      , platform = "x86_64-linux"
      , builder = "/bin/sh"
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
                , renderDownstreamPlaceholder $ downstreamPlaceholderFromSingleDerivedPathBuilt (SingleDerivedPath_Opaque d) (OutputName $ bad "interface")
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
  Node_Link -> fail "not yet implemented"

caOutputSpec :: ContentAddressedDerivationOutput
caOutputSpec = ContentAddressedDerivationOutput
  { caMethod = ContentAddressMethod_NixArchive
  , caHashAlgo = Some HashAlgo_SHA256
  }

outputsFromList :: [OutputName] -> DSum DerivationType (Map OutputName)
outputsFromList outputs = DerivationType_ContentAddressing :=> (Map.fromList $ (\on -> (on, caOutputSpec)) <$> outputs)

bad :: Text -> StorePathName
bad = either (error . show) id . mkStorePathName
