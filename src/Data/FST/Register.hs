module Data.FST.Register(
    Register
  , finalArc
  , empty
  , replaceOrRegister
  , finalize
  ) where

import           Data.FST.Types

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Internal.Write as Blaze
import qualified Blaze.ByteString.Builder.Word as Blaze
import           Data.Bits
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Word (Word32, Word64)
import           Data.HashMap.Strict (HashMap)
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import           Foreign.Storable

data Entry = E !StateRef !Word32
            deriving (Eq, Show)

type TransducerSize = Word64

data Register a = Register {
    regStates   :: !(HashMap [Arc] Entry)
  , regSize     :: !TransducerSize
  , regBuilder  :: !Builder
  }

empty :: Register a
empty = Register {
    regStates  = HashMap.fromList []
  , regSize    = 0
  , regBuilder = mempty
  }

finalArc :: Arc
finalArc = Arc {
    arcLabel    = 0
  , arcNumWords = 1
  , arcTarget   = 0
  }



finalize :: Register a -> ByteString
finalize = Blaze.toByteString . regBuilder

replaceOrRegister ::
  UncompiledState a
  -> Register a
  -> (Arc, Register a)
replaceOrRegister (UncompiledState label arcs output) register =
  case HashMap.lookup arcs (regStates register) of
   Just (E stateRef n) | n == 1    -> (Arc label n stateRef, register)
                       | otherwise -> writeOutput register
   _                   -> (Arc label numWords (regSize register'), register')
  where
    register' = register {
        regStates  = HashMap.insert arcs
                     (E (regSize register + compiledSize) numWords) (regStates register)
      , regSize    = compiledSize + (regSize register)
      , regBuilder = compiledArcs `mappend` regBuilder register
      }

    (numWords, compiledSize, compiledArcs) = compileArcs (regSize register) arcs

writeOutput :: Register a -> (Arc, Register a)
writeOutput register = (arc, register')
  where
    register' = register {
        regSize  = (regSize register)
      , regBuilder = regBuilder register
      }
    arc = Arc 0 0 (regSize register')

compileArcs :: TransducerSize -> [Arc] -> (Weight, Word64, Builder)
compileArcs size arcs =
  (weight, fromIntegral (Blaze.getBound bytes), Blaze.fromWrite bytes)
  where
    (weight, bytes) = case arcs of
                       (arc:[]) -> (arcNumWords arc, compileSimpleArc size arc)
                       _        -> compileMultipleArcs size arcs

compileMultipleArcs :: TransducerSize -> [Arc] -> (Weight, Blaze.Write)
compileMultipleArcs size arcs = (numWords, Blaze.writeWord32be length `mappend` bytes)
  where
    (length, numWords, bytes) = go arcs

    go :: [Arc] -> (Word32, Weight, Blaze.Write)
    go []     = (0, 0, mempty)
    go (a:ax) = (n + 1, weight', write `mappend` write')
      where
        (n, weight, write) = go ax
        weight' = weight + arcNumWords a

        write' = Blaze.writeWord32be (fromIntegral (arcLabel a))
                 `mappend` Blaze.writeWord32be weight
                 `mappend` Blaze.writeWord64be (size - arcTarget a)

compileSimpleArc :: TransducerSize -> Arc -> Blaze.Write
compileSimpleArc size arc | isPrevious = compileSingleNextArc arc
                          | otherwise  = compileSingleArc size arc
  where
    isPrevious = size - arcTarget arc == 0

compileSingleArc :: TransducerSize -> Arc -> Blaze.Write
compileSingleArc size arc = bytes
  where
    label   = fromIntegral (arcLabel arc) :: Word32
    flags   = 0x80000000 :: Word32
    bytes   = Blaze.writeWord32be (label .|. flags)
              `mappend` Blaze.writeWord64be (size - arcTarget arc)

compileSingleNextArc :: Arc -> Blaze.Write
compileSingleNextArc arc = bytes
  where
    label   = fromIntegral (arcLabel arc) :: Word32
    flags   = 0xC0000000 :: Word32
    bytes   = Blaze.writeWord32be (label .|. flags)
