module Sandstone.MakefileParse where

import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Validation
import System.Nix.Derivation
import System.Nix.StorePath

import Sandstone.MakefileGraph

parseModuleLine :: Text -> Maybe ((ModuleName, Text), (ModuleName, Text))
parseModuleLine line = do
  guard $ not $ "#" `T.isPrefixOf` line
  let (target, prereq) = T.breakOn " : " line
  file <- T.stripPrefix " : " prereq
  (moduleName, ext) <- parseFilePath file
  (targetName, targetExt) <- parseFilePath target
  pure ((targetName, targetExt), (moduleName, ext))
 where
  parseFilePath file = do
    let (path, ext) = T.breakOn "." file
    moduleName <- NEL.nonEmpty $ T.splitOn "/" path
    pure (moduleName, ext)

type ErrorForest = NonEmpty ErrorTree

data ErrorTree
  = Error Text
  | Context Text (NonEmpty ErrorTree)
  deriving (Eq, Ord, Show, Read)

addErrorContext :: Text -> Validation ErrorForest a -> Validation ErrorForest a
addErrorContext ctx = first $ NEL.singleton . Context ctx

recordNode :: ((ModuleName, Text), (ModuleName, Text)) -> Validation ErrorForest (Node, [Node])
recordNode ((targetModuleName, eT), (depModuleName, eD)) =
  ((,) <$> targetExt eT <*> depExt eD) `bindValidation` \(f, g) ->
    let
      deps = case g of
        -- Mere source file dep, which we don't track as a dep since it is conditional
        -- However, want to to make sure the node exists if it has no other deps
        Nothing ->
          if targetModuleName == depModuleName
          then pure []
          else Failure $ NEL.singleton $ Error $ T.unwords
            [ "Source file and object file's module names did not match:"
            , T.intercalate "." $ toList targetModuleName
            , T.intercalate "." $ toList depModuleName
            ]
        -- Dependency on the interface of another module
        Just g' -> pure [g' depModuleName]
    in (\ds -> (f targetModuleName, ds)) <$> deps

  where
    targetExt = \case
      -- object file cases
      ".o" -> Success Node_Compile
      ".o-boot" -> Success Node_PreCompile
      -- error case
      x -> Failure $ NEL.singleton $ Error $ T.unwords ["Unrecoginized extension", x, "for target"]

    depExt = \case
      -- source file dep case
      ".hs" -> Success Nothing
      ".hs-boot" -> Success Nothing
      -- interface file dep cases
      ".hi" -> Success $ Just $ Node_Compile
      ".hi-boot" -> Success $ Just $ Node_PreCompile
      -- error case
      x -> Failure $ NEL.singleton $ Error $ T.unwords ["Unrecoginized extension", x, "for dependency"]
