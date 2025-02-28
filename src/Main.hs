module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy as BSL
import Data.Default
import Data.Dependent.Sum
import Data.Graph
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Some
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
import System.Process (readProcess)

import Sandstone.Error
import Sandstone.MakefileParse qualified as MakefileParse

main :: IO ()
main = do
  result <- nixStoreAdd "example/Makefile"
  print result

  result2 <- nixDerivationAdd exampleDrv
  print result2

  makefile <- T.readFile "example/Makefile"
  print makefile
  let moduleLines0 = catMaybes $ MakefileParse.parseModuleLine <$> T.lines makefile
  mapM_ print moduleLines0
  nodes <- case
      traverse
       (\line -> addErrorContext
         ("While recording node for Makefile line: " <> T.pack (show line))
         $ MakefileParse.recordNode line)
       moduleLines0
    of
      Failure e -> fail $ show e
      Success a -> pure a
  let (graph, _) = graphFromEdges' $ fmap (\(node, edges') -> (node, node, edges')) nodes
  print graph

objectPlaceholder, interfacePlaceholder :: Text

-- | For the output called "object" of the derivation that uses this placeholder
objectPlaceholder = "/07fvp8gkd5mhhfi1lqjfwq7sxnpmdfczz27lizfxiz6fpwad8sy4"

-- | For the output called "interface" of the derivation that uses this placeholder
interfacePlaceholder = "/1ang7n5l91vn079693l42ahmcxgf34r0qad1l01y4lf7d3cwm5lg"

exampleDrv :: Derivation
exampleDrv = Derivation
  { name = bad "hello-from-sandstone"
  , outputs = DerivationType_ContentAddressing :=> let
      outputSpec = ContentAddressedDerivationOutput
        { caMethod = ContentAddressMethod_NixArchive
        , caHashAlgo = Some HashAlgo_SHA256
        }
    in Map.fromList $ (\on -> (OutputName $ bad on, outputSpec)) <$> ["object", "interface"]
  , inputs = DerivationInputs
    { srcs = Set.empty -- /nix/store/asdfasdfasdf-Foo.hs will go here
    , drvs = mempty -- /nix/store/asdfasdfasdf-Foo.drv^interface will go here
    }
  , platform = "x86_64-linux"
  , builder = "echo"
  , args = V.fromList ["-c", "set -xeu; echo" <> "" <> " >> $object ;" ]
  , env = Map.empty
  }

 where
   bad = either (error . show) id . mkStorePathName

nixStoreAdd :: FilePath -> IO (Either InvalidPathError StorePath)
nixStoreAdd fp = do
  str <- readProcess "nix-store" (localStoreArgs <> ["--add", fp]) ""
  pure $ parsePathFromText def $ T.strip $ T.pack str

nixDerivationAdd :: Derivation -> IO (Either InvalidPathError StorePath)
nixDerivationAdd drv = do
  let drvJson = T.decodeUtf8 $ BSL.toStrict $ Aeson.encode drv
  str <- readProcess "nix" (localStoreArgs <> [ "derivation", "add"]) $ T.unpack drvJson
  pure $ parsePathFromText def $ T.strip $ T.pack str

localStoreArgs :: [String]
localStoreArgs = ["--store", "/tmp/sand", "--extra-experimental-features", "ca-derivations"]
