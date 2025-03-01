module Sandstone.MakefileGraph where

import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Data.List.NonEmpty (NonEmpty)

-- | E.g. "Foo.Bar.Baz" becomes '["Foo", "Bar", "Baz"]'
type ModuleName = NonEmpty Text

-- | The key for a module proper, or boot module interface
data Module = Module
  -- | Whether we mean the module proper, or its boot interface
  --
  -- TODO backpack sigs
  { hsBoot :: Bool
  -- | It's name
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

-- | Path without file extension
pathNoExt :: Module -> FilePath
pathNoExt module' = T.unpack $ T.intercalate "/" (toList $ moduleName module')

-- | Source file extension
sourceExt :: Module -> Text
sourceExt module' =
  if hsBoot module' then "hs-boot" else "hs"

-- | Interface file extension
interfaceExt :: Module -> Text
interfaceExt module' =
  if hsBoot module' then "hi-boot" else "hi"

-- | Object file extension
objectExt :: Module -> Text
objectExt module' =
  if hsBoot module' then "o-boot" else "o"
