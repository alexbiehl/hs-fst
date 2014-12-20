module Data.FSA.Register.Binary where

import           Data.FSA.Types
import           Data.FSA.Register.Binary.Write



import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Internal.Write
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid
import           Data.Word (Word16, Word32, Word64)

data FST = FST StateRef ByteString

data BinaryRegister = BinaryRegister {
    regStates       :: !(HashMap ByteString StateRef)
  , regOut          :: !Builder
  , regLastStateRef :: !StateRef
  , regLastStateSz  :: !Word64
  }

empty :: BinaryRegister
empty = BinaryRegister {
    regStates       = mempty
  , regOut          = mempty
  , regLastStateRef = 0
  , regLastStateSz  = 1
  }

finalize :: BinaryRegister -> FST
finalize register = FST rootRef body
  where
    rootRef = regLastStateRef register
    body    = toByteString (regOut register)

lookup :: [Word16] -> FST -> Maybe ()
lookup word (FST rootRef body) = undefined

replaceOrRegister :: Compiler BinaryRegister
replaceOrRegister arcs register =
  case HashMap.lookup state (regStates register) of
    Just ref -> (ref, register)
    Nothing  -> (statePos, register')
  where
    state    = writeArcs arcs
    statePos = regLastStateSz register + regLastStateRef register
    register' = BinaryRegister {
      regStates       = HashMap.insert state statePos (regStates register)
    , regOut          = regOut register `mappend` insertByteString state
    , regLastStateRef = statePos
    , regLastStateSz  = fromIntegral (ByteString.length state)
    }
