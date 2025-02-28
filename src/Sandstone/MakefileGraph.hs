module Sandstone.MakefileGraph where

import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Data.List.NonEmpty (NonEmpty)

type ModuleName = NonEmpty Text

data Module = Module
  { hsBoot :: Bool
  , moduleName :: ModuleName
  }
  deriving (Show, Ord, Eq)

-- | See https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html
data Node
  -- | if 'hsBoot'
  --
  -- - then for `hs-boot` file into `.o-boot` (mostly fake) and `.hi-boot` files,
  --
  -- - else for `hs` file into `o` and `hi` files
  --
  -- Dependencies on other modules are always on their hi file outputs
  = Node_Compile Module
  -- | Not sure how to get from Makefile
  --
  -- Dependencies on other modules are always on their o file outputs
  | Node_Link
  deriving (Show, Ord, Eq)

pathNoExt :: Module -> FilePath
pathNoExt module' = T.unpack $ T.intercalate "/" (toList $ moduleName module')

sourceExt :: Module -> Text
sourceExt module' =
  if hsBoot module' then "hs-boot" else "hs"

interfaceExt :: Module -> Text
interfaceExt module' =
  if hsBoot module' then "hi-boot" else "hi"

objectExt :: Module -> Text
objectExt module' =
  if hsBoot module' then "o-boot" else "o"
