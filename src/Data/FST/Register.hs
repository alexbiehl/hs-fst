module Data.FST.Register where

import Data.FST.Arcs
import Data.Word

data UncompiledState = UncompiledState {
    ucLabel :: !Word8
  , ucArcs  :: !Arcs
  } deriving (Eq, Show)

type ReplaceOrRegister s r = UncompiledState -> s -> (Arc -> s -> r) -> r
