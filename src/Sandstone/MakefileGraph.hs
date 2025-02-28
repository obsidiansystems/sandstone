{-# LANGUAGE NoFieldSelectors #-}
module Sandstone.MakefileGraph where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

type ModuleName = NonEmpty Text

-- | See https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html
data Node
  -- | For `hs` file into `o` and `hi` files
  --
  -- Dependencies on other modules are always on their hi file outputs
  = Node_Compile { moduleName :: ModuleName }
  -- | For `hs-boot` file into `.o-boot` (mostly fake) and `.hi-boot` files
  --
  -- Dependencies on other modules are always on their hi file outputs
  | Node_PreCompile { moduleName :: ModuleName }
  -- | Not sure how to get from Makefile
  --
  -- Dependencies on other modules are always on their o file outputs
  | Node_Link
  deriving (Show, Ord, Eq)
