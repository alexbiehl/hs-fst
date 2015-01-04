{-# LANGUAGE OverloadedStrings #-}
module Data.FST.Register.Dot where

import           Data.FST.Types

import           Data.Char (chr)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import           Data.ByteString.Builder
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Monoid

data Dotted s a = Dotted {
    dotState :: !s
  , dotRor   :: !(ReplaceOrRegister s a)
  , dotBuild :: !Builder
  }

empty :: s -> ReplaceOrRegister s a -> Dotted s a
empty s0 ror = Dotted s0 ror mempty

showDot :: Dotted s a -> ByteString
showDot = toLazyByteString . dotBuild

dotted :: ReplaceOrRegister (Dotted s a) a
dotted node dot = (arc, dot')
  where
    (arc@(Arc _ _ stateRef), s') = dotRor dot node (dotState dot)

    dot' = dot {
        dotState = s'
      , dotBuild = dotBuild dot `mappend` shown
      }

    shown = mconcat $ map (dotArcFromTo stateRef) (ucArcs node)

dotInit :: Builder
dotInit = mconcat [
      "digraph finite_state_machine {\n"
    , "rankdir=LR\n"
    , "node [shape = doublecircle]; 0;\n"
    , "node [shape = circle];\n"
    ]

dotEnd :: Builder
dotEnd = mconcat [
    "}"
  ]

dotArcFromTo :: StateRef -> Arc -> Builder
dotArcFromTo from (Arc label numWords to) =
 mconcat [
   word64Dec from
 , " -> "
 , word64Dec to
 , " [ label=\""
 , correctedLabel
 , "/#"
 , word32Dec numWords
 , "\" ];\n"
 ]
 where
  correctedLabel = if to == 0
                    then "\\$"
                    else char8 (chr (fromIntegral label))
