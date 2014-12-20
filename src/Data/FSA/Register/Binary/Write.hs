module Data.FSA.Register.Binary.Write where

import Data.FSA.Types

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Word
import           Blaze.ByteString.Builder.Internal.Write
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Monoid

writeArc :: Arc -> Write
writeArc (Arc word to) =
  writeWord16be word `mappend` writeWord64be to

writeArcs :: [Arc] -> ByteString
writeArcs arcs = writeToByteString write
  where
    len   = getBound write `div` 10
    write = mconcat $ (writeWord16be (fromIntegral len)):(fmap writeArc arcs)