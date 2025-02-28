module Sandstone.Error where

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)
import Data.Validation

type ErrorForest = NonEmpty ErrorTree

data ErrorTree
  = Error Text
  | Context Text (NonEmpty ErrorTree)
  deriving (Eq, Ord, Show, Read)

addErrorContext :: Text -> Validation ErrorForest a -> Validation ErrorForest a
addErrorContext ctx = first $ NEL.singleton . Context ctx

