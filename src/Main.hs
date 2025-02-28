{-# Language NamedFieldPuns #-}
module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy as BSL
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
import System.Nix.ContentAddress
import System.Nix.Derivation
import System.Nix.JSON ()
import System.Nix.Hash
import System.Nix.OutputName
import System.Nix.StorePath
import System.Nix.DerivedPath
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
  memo <- flip execStateT Map.empty $ mapM_ (uncurry $ writeDerivation') todo

  nixStoreRealise $ (memo Map.!) $ (\(a, _, _) -> a) $ lookupVertex $ Prelude.head $ topSort graph

type DrvMemo = Map Node StorePath


writeDerivation' :: Node -> [Node] -> StateT DrvMemo IO ()
writeDerivation' node deps = do
  memo <- get
  drvPath <- lift $ writeDerivation memo node deps
  modify $ Map.insert node drvPath

writeDerivation :: DrvMemo -> Node -> [Node] -> IO StorePath
writeDerivation memo node deps = case node of
  Node_Compile module' -> do
    print module'

    Right source <- nixStoreAdd
      ("example/" <> sourcePath module')
      (T.intercalate "." (NEL.toList $ moduleName module') <> "." <> sourceExt module')
    print source

    Just deps' <- pure $ traverse (flip Map.lookup memo) deps

    Right result2 <- nixDerivationAdd $ Derivation
      { name = bad $ "compile-" <> T.intercalate "." (NEL.toList $ moduleName module')
      , outputs = outputsFromList ["object", "interface"]
      , inputs = foldMap
          derivationInputsFromSingleDerivedPath
          $ SingleDerivedPath_Opaque source
          : (flip SingleDerivedPath_Built (OutputName $ bad "interface") . SingleDerivedPath_Opaque <$> deps')
      , platform = "x86_64-linux"
      , builder = "echo"
      , args = V.fromList ["-c", "set -xeu; echo" <> "" <> " >> $object ;" ]
      , env = Map.empty
      }
    print result2
    pure result2
  Node_Link -> fail "not yet implemented"


objectPlaceholder, interfacePlaceholder :: Text

-- | For the output called "object" of the derivation that uses this placeholder
objectPlaceholder = "/07fvp8gkd5mhhfi1lqjfwq7sxnpmdfczz27lizfxiz6fpwad8sy4"

-- | For the output called "interface" of the derivation that uses this placeholder
interfacePlaceholder = "/1ang7n5l91vn079693l42ahmcxgf34r0qad1l01y4lf7d3cwm5lg"

caOutputSpec :: ContentAddressedDerivationOutput
caOutputSpec = ContentAddressedDerivationOutput
  { caMethod = ContentAddressMethod_NixArchive
  , caHashAlgo = Some HashAlgo_SHA256
  }

outputsFromList :: [Text] -> DSum DerivationType (Map OutputName)
outputsFromList outputs = DerivationType_ContentAddressing :=> (Map.fromList $ (\on -> (OutputName $ bad on, caOutputSpec)) <$> outputs)

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
  callProcess "nix" (localStoreArgs <> ["build", T.unpack $ storePathToText storeDir fp <> "^*", "-v"])

nixDerivationAdd :: Derivation -> IO (Either InvalidPathError StorePath)
nixDerivationAdd drv = do
  let drvJson = T.decodeUtf8 $ BSL.toStrict $ Aeson.encode drv
  str <- readProcess "nix" (localStoreArgs <> [ "derivation", "add"]) $ T.unpack drvJson
  pure $ parsePathFromText storeDir $ T.strip $ T.pack str

localStoreArgs :: [String]
localStoreArgs =
  [ "--store", "/tmp/sand"
  , "--extra-experimental-features", "nix-command ca-derivations"
  , "--substituters", "http://cache.nixos.org"
  ]
