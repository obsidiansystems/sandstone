{-# LANGUAGE NoFieldSelectors #-}
module Graph where

import Data.Graph
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

type ModuleName = NonEmpty Text

data Node
  = Node_Compile { moduleName :: ModuleName }
  -- | For `hs-boot` file
  | Node_PreCompile { moduleName :: ModuleName }
  -- | Not sure how to get from Makefile
  | Node_Link

data Edge
  -- | For .hi/.hi-boot
  = Edge_Interface Node
  -- | For .o
  --
  -- N.B./ .o-boot
  | Edge_Object Node
