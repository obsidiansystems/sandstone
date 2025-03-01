{-# Language NamedFieldPuns #-}
module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Default
import Data.Dependent.Sum
import Data.Graph
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Some
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
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
import System.Process hiding (env)

import Sandstone.MakefileGraph
import Sandstone.MakefileParse

main :: IO ()
main = do
  makefile <- T.readFile "example/Makefile"
  print makefile

  (graph, lookupVertex) <- case parseMakefile makefile of
    Failure e -> fail $ show e
    Success a -> pure a

  let todo = fmap ((\(a, _, b) -> (a, b)) . lookupVertex) $ reverseTopSort graph

  Right ghcDrvPath <- nixIntantiateInDepNixpkgs "ghc"
  Right coreutilsDrvPath <- nixIntantiateInDepNixpkgs "coreutils"
  Right lndirDrvPath <- nixIntantiateInDepNixpkgs "xorg.lndir"

  putStrLn "done with eval"

  let ctx = Ctx
       { ghcDrvPath = SingleDerivedPath_Opaque ghcDrvPath
       , coreutilsDrvPath = SingleDerivedPath_Opaque coreutilsDrvPath
       , lndirDrvPath = SingleDerivedPath_Opaque lndirDrvPath
       }

  memo <- flip execStateT Map.empty $ mapM_ (uncurry $ writeDerivation' ctx) todo

  nixStoreRealise $ (memo Map.!) $ (\(a, _, _) -> a) $ lookupVertex $ Prelude.head $ topSort graph

type DrvMemo = Map Node StorePath

data Ctx = Ctx
  { ghcDrvPath :: SingleDerivedPath
  , coreutilsDrvPath :: SingleDerivedPath
  , lndirDrvPath :: SingleDerivedPath
  } deriving (Eq, Ord, Show)

writeDerivation' :: Ctx -> Node -> [Node] -> StateT DrvMemo IO ()
writeDerivation' ctx node deps = do
  memo <- get
  drvPath <- lift $ writeDerivation ctx memo node deps
  modify $ Map.insert node drvPath

interface, object :: OutputName
object = OutputName $ bad "object"
interface = OutputName $ bad "interface"

writeDerivation :: Ctx -> DrvMemo -> Node -> [Node] -> IO StorePath
writeDerivation ctx memo node deps = case node of
  Node_Compile module' -> do
    print module'

    Right source <- nixStoreAdd
      ("example/" <> pathNoExt module' <> "." <> T.unpack (sourceExt module'))
      (T.intercalate "." (NEL.toList $ moduleName module') <> "." <> sourceExt module')
    print source

    putStrLn "==> CTX:"
    print ctx
    putStrLn "CTX <=="

    Just deps' <- pure $ traverse (flip Map.lookup memo) deps
    putStrLn "==> DEPS:"
    print deps'
    putStrLn "DEPS <=="


    let ghcPlaceholder = renderDownstreamPlaceholder $ downstreamPlaceholderFromSingleDerivedPathBuilt (ghcDrvPath ctx) (OutputName $ bad "out")
    let coreutilsPlaceholder = renderDownstreamPlaceholder $ downstreamPlaceholderFromSingleDerivedPathBuilt (coreutilsDrvPath ctx) (OutputName $ bad "out")
    let lndirPlaceholder = renderDownstreamPlaceholder $ downstreamPlaceholderFromSingleDerivedPathBuilt (lndirDrvPath ctx) (OutputName $ bad "out")

    Right result2 <- nixDerivationAdd $ Derivation
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
    print result2
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
