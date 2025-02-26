{-# LANGUAGE NoFieldSelectors #-}
module Graph where

import Data.Graph
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

type ModuleName = NonEmpty Text

-- | See https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html
data Node
  = Node_Compile { moduleName :: ModuleName }
  -- | For `hs-boot` file
  | Node_PreCompile { moduleName :: ModuleName }
  -- | Not sure how to get from Makefile
  | Node_Link

data Edge
  -- | For .hi/.hi-boot
  = Edge_Interface Node
  -- | For .o/.o-boot
  --
  -- N.B. .o-boot is pretty fake.
  | Edge_Object Node
