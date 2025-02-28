module Main where

import Data.Dependent.Sum
import Data.Graph
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Some
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Validation
import Data.Vector qualified as V
import System.Nix.ContentAddress
import System.Nix.Derivation
import System.Nix.Hash
import System.Nix.OutputName
import System.Nix.StorePath

import Sandstone.Error
import Sandstone.MakefileParse qualified as MakefileParse

main :: IO ()
main = do
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
  pure ()
  let (graph, _) = graphFromEdges' $ fmap (\(node, edges') -> (node, node, edges')) nodes
  print graph


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
  , args = V.fromList ["-c"]
  , env = Map.empty
  }

 where
   bad = either (error . show) id . mkStorePathName
