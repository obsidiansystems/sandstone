module Sandstone.Error where

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

type ErrorForest = NonEmpty ErrorTree

data ErrorTree
  = Error Text
  | Context Text (NonEmpty ErrorTree)
  deriving (Eq, Ord, Show, Read)

addErrorContext :: Text -> Validation ErrorForest a -> Validation ErrorForest a
addErrorContext ctx = first $ NEL.singleton . Context ctx

