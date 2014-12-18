module Data.FSA.Types where

import Data.Word (Word16, Word64)
import Data.Hashable


-- | A `StateRef` is a stable reference to a state in the register while compiling
type StateRef = Word64

-- | `Arc` defines a transition with `arcByte` to `arcTo`
data Arc = Arc {
   arcByte :: !Word16
 , arcTo   :: !StateRef
} deriving (Eq, Show)

-- | A `Compiler` maps a list of `Arc`s to a corresponding `StateRef`.
--   For minimality to hold, a `Compiler` has to return the same `StateRef`
--   for all equal [`Arc`].
type Compiler a = [Arc] -> a -> (StateRef, a)

instance Hashable Arc where
  hashWithSalt s (Arc b t) = s `hashWithSalt` t `hashWithSalt` b
