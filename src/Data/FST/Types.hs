module Data.FST.Types where

import Blaze.ByteString.Builder (Builder)
import Data.Word (Word8, Word32, Word64)
import Data.Hashable

type StateRef = Word64

data Arc = Arc {
    arcLabel    :: !Word8
  , arcNumWords :: !Word32
  , arcTarget   :: !StateRef
  } deriving (Eq, Show)

data UncompiledState a = UncompiledState {
    ucLabel  :: !Word8
  , ucArcs   :: ![Arc]
  , ucOutput :: !(Maybe a)
  } deriving (Eq, Show)

data Node a = Node ![Arc] !(Maybe a)
            deriving (Eq, Show)

type ReplaceOrRegister s a = UncompiledState a -> s -> (Arc, s)

instance Hashable Arc where
  hashWithSalt s (Arc w l st) =
    s `hashWithSalt` w `hashWithSalt` l `hashWithSalt` st
