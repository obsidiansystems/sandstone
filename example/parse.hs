{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Monad (guard)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

main :: IO ()
main = do
  makefile <- T.readFile "Makefile"
  print makefile
  print $ catMaybes $ parseModuleLine <$> T.lines makefile
  pure ()

parseModuleLine :: Text -> Maybe Text
parseModuleLine line = do
  guard $ not $ "#" `T.isPrefixOf` line
  let (l, r) = T.breakOn " : " line
  pure r
