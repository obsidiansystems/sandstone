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

recordNode :: ((ModuleName, Text), (ModuleName, Text)) -> Maybe (Node, [Edge])
recordNode = \case
  ((moduleName, ".o"), (moduleName', ".hs")) 
    | moduleName == moduleName' -> Just (Node_Compile moduleName, [])
  ((moduleName, ".o-boot"), (moduleName', ".hs")) 
    | moduleName == moduleName' -> Just (Node_PreCompile moduleName, [])
  ((moduleName, ".o"), (interfaceName, ".hi")) -> 
    Just (Node_Compile moduleName, [Edge_Interface $ Node_Compile interfaceName])
  ((moduleName, ".o-boot"), (interfaceName, ".hi")) ->
    Just (Node_PreCompile moduleName, [Edge_Interface $ Node_Compile interfaceName])
  _ -> Nothing
