module Data.FST.Register(
    Register
  , finalArc
  , empty
  , replaceOrRegister
  ) where

import Data.FST.Types
import Foreign.Storable
import Data.Word (Word32)
import Data.HashMap.Strict (HashMap)
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List

data Register a = Register {
    regStates   :: !(HashMap [Arc] StateRef)
  , regNextRef  :: !StateRef
  } deriving (Eq, Show)

empty :: Register a
empty = Register {
    regStates  = HashMap.fromList []
  , regNextRef = 1
  }

finalArc :: Arc
finalArc = Arc {
    arcLabel    = 0
  , arcNumWords = 1
  , arcTarget   = 0
  }

replaceOrRegister ::
  UncompiledState a
  -> Register a
  -> (Arc, Register a)
replaceOrRegister node register =
  case HashMap.lookup arcs (regStates register) of
   Just stateRef -> (Arc label numWords stateRef, register)
   Nothing       -> (Arc label numWords (regNextRef register), register')
  where
    UncompiledState label arcs output = node

    register' = register {
        regStates  = HashMap.insert arcs
                     (regNextRef register) (regStates register)
      , regNextRef = (regNextRef register) + 1
      }

    numWords = Foldable.foldl' (+) 0
               $ fmap arcNumWords
               $ arcs
