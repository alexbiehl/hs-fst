module Data.FSA.Register.Binary.Lookup where

import Data.FSA.Types

import Data.ByteString (ByteString)
import Data.ByteString.Internal
import Data.Word (Word16)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

lookup :: [Word16] -> StateRef -> ByteString -> Maybe ()
lookup word root (PS fp off len) =
  inlinePerformIO $ withForeignPtr fp $ \base -> do
    let ptr = base `plusPtr` off
        end = ptr  `plusPtr` len
    go word ptr end
  where
    go (w:wx) ptr endPtr = do
      undefined
