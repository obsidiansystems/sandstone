{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseMakefile where

import Control.Monad (guard, mapM_)
import Data.List.NonEmpty
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Graph

main :: IO ()
main = do
  makefile <- T.readFile "example/Makefile"
  print makefile
  let modules = catMaybes $ parseModuleLine <$> T.lines makefile
  mapM_ print modules

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
    moduleName <- nonEmpty $ T.splitOn "/" path
    pure (moduleName, ext)

recordNode :: ((ModuleName, Text), (ModuleName, Text)) -> Either Text (Node, [Edge])
recordNode ((targetModuleName, eT), (depModuleName, eD)) = do
    f <- targetExt eT
    g <- depExt eD
    deps <- case g of
      -- Mere source file dep, which we don't track as a dep since it is conditional
      -- However, want to to make sure the node exists if it has no other deps
      Nothing ->
        if targetModuleName == depModuleName
        then pure []
        else Left $ T.unwords
          [ "Source file and object file's module names did not match:"
          , T.intercalate "." $ toList targetModuleName
          , T.intercalate "." $ toList depModuleName
          ]
      Just g' -> pure [g' depModuleName]
    pure (f targetModuleName, deps)

  where
    targetExt = \case
      ".o" -> Right Node_Compile
      ".o-boot" -> Right Node_PreCompile
      x -> Left $ T.unwords ["Unrecoginized extension", x, "for target"]

    depExt = \case
      ".hs" -> Right $ Nothing
      ".hi" -> Right $ Just $ Edge_Interface . Node_Compile
      ".hi-boot" -> Right $ Just $ Edge_Interface . Node_PreCompile
      x -> Left $ T.unwords ["Unrecoginized extension", x, "for dependency"]
