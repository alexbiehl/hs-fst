module Data.FSA.Types where

import Data.Word (Word16, Word64)
import Data.Hashable

type StateRef = Word64

data Arc = Arc {
   arcByte :: !Word16
 , arcTo   :: !StateRef
} deriving (Eq, Show)

type RootArcs = [Arc]

type Compiler a = [Arc] -> a -> (StateRef, a)

instance Hashable Arc where
  hashWithSalt s (Arc b t) = hashWithSalt (hashWithSalt s b) t
