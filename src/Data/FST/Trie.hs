{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Data.FST.Trie where

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Internal.Write as Blaze
import           Control.DeepSeq
import           Data.Bits
import           Data.ByteString (ByteString)
import           Data.FST.Arcs (Arc(..), Arcs)
import qualified Data.FST.Arcs as Arcs
import           Data.FST.Register
import           Data.FST.Types
import qualified Data.List as List
import           Data.Monoid
import           Data.Word

newtype Trie = Trie { unTrie :: ByteString }
               deriving (Eq, Show, NFData)

data Register = Register {
    regOffset :: !Offset
  , regWeight :: !Weight
  , regBuffer :: !Builder
  }

instance Register1 Register where
  type Output Register = Trie
  empty             = Data.FST.Trie.empty
  output            = Data.FST.Trie.output
  replaceOrRegister = Data.FST.Trie.replaceOrRegister

type Offset = Word64

empty :: Register
empty = Register 0 0 mempty

output :: Register -> Trie
output = Trie . Blaze.toByteString . regBuffer
{-# INLINE output #-}

isFinalArc :: Arc -> Bool
isFinalArc = (Arcs.final ==)
{-# INLINE isFinalArc #-}

replaceOrRegister :: UncompiledState -> Register -> (Arc -> Register -> a) -> a
replaceOrRegister (UncompiledState label arcs) register k =
  k (Arc label 0 (regOffset register')) register'
  where
    register' = register {
        regOffset = offset'
      , regWeight = regWeight register + if label == 0 then 1 else 0
      , regBuffer = buffer'
      }

    offset'    = (regOffset register) + fromIntegral n
    buffer'    = bytes `mappend` regBuffer register
    (n, bytes) = case label of
      0 -> compileFinalArc (regWeight register)
      _ -> compileArcs (regOffset register) arcs
{-# INLINE replaceOrRegister #-}

compileFinalArc :: Weight -> (Int, Builder)
compileFinalArc weight = (Blaze.getBound bytes, Blaze.fromWrite bytes)
 where
   bytes =  Blaze.writeWord16be 0
           `mappend` Blaze.writeWord32be weight
{-# INLINE compileFinalArc #-}

compileArcs :: Offset -> Arcs -> (Int, Builder)
compileArcs offset arcs = (Blaze.getBound bytes, Blaze.fromWrite bytes)
  where
    bytes = case Arcs.length arcs of
      1 -> compileSimpleArc offset (List.head (Arcs.arcs arcs))
      _ -> compileMultipleArcs offset arcs
{-# INLINE compileArcs #-}

compileMultipleArcs :: Offset -> Arcs -> Blaze.Write
compileMultipleArcs offset arcs =
  Blaze.writeWord32be (fromIntegral $ Arcs.length arcs)
  `mappend` go (Arcs.arcs arcs)
  where
    go :: [Arc] -> Blaze.Write
    go []     = mempty
    go (Arc label _ target : ax) =
      go ax
      `mappend` Blaze.writeWord16be (fromIntegral label)
      `mappend` Blaze.writeWord64be (offset - target)
{-# INLINE compileMultipleArcs #-}

compileSimpleArc :: Offset -> Arc -> Blaze.Write
compileSimpleArc offset arc | isPrevious = compileSingleNextArc arc
                            | otherwise  = compileSingleArc offset arc
  where
    isPrevious = offset - arcTarget arc == 0
{-# INLINE compileSimpleArc #-}

compileSingleArc :: Offset -> Arc -> Blaze.Write
compileSingleArc offset (Arc label _ target) =
  Blaze.writeWord16be word `mappend` Blaze.writeWord64be (offset - target)
  where
    word :: Word16
    word = 0x8000 .|. (fromIntegral label .&. 0x00F)
{-# INLINE compileSingleArc #-}

compileSingleNextArc :: Arc -> Blaze.Write
compileSingleNextArc (Arc label _ _) = Blaze.writeWord16be word
  where
    word :: Word16
    word = 0xC000 .|. (fromIntegral label .&. 0x00FF)
{-# INLINE compileSingleNextArc #-}
