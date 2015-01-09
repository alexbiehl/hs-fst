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
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Word (Word32)
import           Data.HashMap.Strict (HashMap)
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import           Foreign.Storable

data Entry = E !StateRef !Word32
            deriving (Eq, Show)

data Register a = Register {
    regStates   :: !(HashMap [Arc] Entry)
  , regNextRef  :: !StateRef
  , regBuilder  :: !Builder
  }

empty :: Register a
empty = Register {
    regStates  = HashMap.fromList []
  , regNextRef = 1
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
   Just (E stateRef n) -> (Arc label n stateRef, register)
   Nothing             -> (Arc label numWords (regNextRef register), register')
  where
    register' = register {
        regStates  = HashMap.insert arcs
                     (E (regNextRef register) numWords) (regStates register)
      , regNextRef = (regNextRef register) + 1
      , regBuilder = compileArcs arcs `mappend` regBuilder register
      }

    numWords = Foldable.foldl' (+) 0
               $ fmap arcNumWords
               $ arcs

compileArcs :: [Arc] -> Builder
compileArcs arcs = Blaze.fromWrite final
  where
    (arcCount, _, write) = go arcs
    final                =
      Blaze.writeWord32be arcCount `mappend` write

    go :: [Arc] -> (Word32, Word32, Blaze.Write)
    go []     = (0, 0, mempty)
    go (a:ax) = (n', weight', built')
      where
        (n, weight, built) = go ax
        n'              = n + 1
        weight'         = arcNumWords a + weight
        built'          = built `mappend` arc
        arc             = Blaze.writeWord8 (arcLabel a)
          `mappend` Blaze.writeWord32be weight'
          `mappend` Blaze.writeWord64be (arcTarget a)