module Main where

import Data.Graph
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Validation

import Sandstone.MakefileParse qualified as MakefileParse

main :: IO ()
main = do
  makefile <- T.readFile "example/Makefile"
  print makefile
  let moduleLines0 = catMaybes $ MakefileParse.parseModuleLine <$> T.lines makefile
  mapM_ print moduleLines0
  nodes <- case
      traverse
       (\line -> MakefileParse.addErrorContext
         ("While recording node for Makefile line: " <> T.pack (show line))
         $ MakefileParse.recordNode line)
       moduleLines0
    of
      Failure e -> fail $ show e
      Success a -> pure a
  pure ()
  let (graph, _) = graphFromEdges' $ fmap (\(node, edges) -> (node, node, edges)) nodes
  print graph

