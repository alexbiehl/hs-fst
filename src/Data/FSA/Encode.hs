module FSA.Encode where

import FSA.Types

import qualified Control.Foldl as L
import Control.Applicative
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Word
import Blaze.ByteString.Builder.Internal.Write
import Data.Monoid
import Data.Foldable (foldl')

writeArc :: Arc -> Write
writeArc (Arc b stateRef) = writeWord8 b `mappend` writeWord64be stateRef

writeArcs :: [Arc] -> Write
writeArcs = undefined