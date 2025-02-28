module Sandstone.MakefileParse where

import Control.Monad
import Data.Foldable
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Validation
import Data.Graph

import Sandstone.Error
import Sandstone.MakefileGraph

parseMakefile :: Text -> Validation ErrorForest (Graph, Vertex -> (Node, Node, [Node]))
parseMakefile makefile =
  let
    moduleLines0 = catMaybes $ parseModuleLine <$> T.lines makefile
    moduleLines1 = traverse
      (\line -> addErrorContext
        ("While recording node for Makefile line: " <> T.pack (show line))
        $ recordNode line)
      moduleLines0
    nodeMap = Map.fromListWith (<>) <$> moduleLines1
  in
    graphFromEdges' . fmap (\(node, edges') -> (node, node, Set.toList edges')) . Map.toList <$> nodeMap

parseModuleLine :: Text -> Maybe ((ModuleName, Text), (ModuleName, Text))
parseModuleLine line = do
  guard $ not $ "#" `T.isPrefixOf` line
  let (target, prereq) = T.breakOn " : " line
  file <- T.stripPrefix " : " prereq
  (moduleName', ext) <- parseFilePath file
  (targetName, targetExt) <- parseFilePath target
  pure ((targetName, targetExt), (moduleName', ext))
 where
  parseFilePath file = do
    let (path', ext) = T.breakOn "." file
    moduleName' <- NEL.nonEmpty $ T.splitOn "/" path'
    pure (moduleName', ext)

recordNode :: ((ModuleName, Text), (ModuleName, Text)) -> Validation ErrorForest (Node, Set Node)
recordNode ((targetModuleName, eT), (depModuleName, eD)) =
  ((,) <$> targetExt eT <*> depExt eD) `bindValidation` \(f, g) ->
    let
      deps = case g of
        -- Mere source file dep, which we don't track as a dep since it is conditional
        -- However, want to to make sure the node exists if it has no other deps
        Nothing ->
          if targetModuleName == depModuleName
          then pure $ Set.empty
          else Failure $ NEL.singleton $ Error $ T.unwords
            [ "Source file and object file's module names did not match:"
            , T.intercalate "." $ toList targetModuleName
            , T.intercalate "." $ toList depModuleName
            ]
        -- Dependency on the interface of another module
        Just g' -> pure $ Set.singleton $ g' depModuleName
    in (\ds -> (f targetModuleName, ds)) <$> deps

  where
    targetExt = \case
      -- object file cases
      ".o" -> Success $ Node_Compile . Module False
      ".o-boot" -> Success $ Node_Compile . Module True
      -- error case
      x -> Failure $ NEL.singleton $ Error $ T.unwords ["Unrecoginized extension", x, "for target"]

    depExt = \case
      -- source file dep case
      ".hs" -> Success Nothing
      ".hs-boot" -> Success Nothing
      -- interface file dep cases
      ".hi" -> Success $ Just $ Node_Compile . Module False
      ".hi-boot" -> Success $ Just $ Node_Compile . Module True
      -- error case
      x -> Failure $ NEL.singleton $ Error $ T.unwords ["Unrecoginized extension", x, "for dependency"]
