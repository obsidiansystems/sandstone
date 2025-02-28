module Main where

import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Validation

import Sandstone.ParseMakefile qualified as ParseMakefile

main :: IO ()
main = do
  makefile <- T.readFile "example/Makefile"
  print makefile
  let moduleLines0 = catMaybes $ ParseMakefile.parseModuleLine <$> T.lines makefile
  mapM_ print moduleLines0
  nodes <- case traverse ParseMakefile.recordNode moduleLines0 of
    Failure e -> fail $ show e
    Success a -> pure a
  pure ()
  -- let (graph, _) = graphFromEdges' $ fmap (\(node, edges) -> (node, undefined, edges)) flattenedGraph
  -- print graph

