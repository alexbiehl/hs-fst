{-# LANGUAGE OverloadedStrings #-}
module Data.FSA.Register.Dot where

import Data.FSA.Types

import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Char (chr)

data Dotted a = Dotted {
    baseCompiler :: !(Compiler a)
  , base         :: !a
  , rendered     :: !(HashSet (StateRef, Arc))
  , dotBuilder   :: !Builder
  }

mkDotted :: Compiler a -> a -> Dotted a
mkDotted compiler initial = Dotted compiler initial HashSet.empty dotInit

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

buildDot :: Dotted a -> (Builder, a)
buildDot (Dotted _ base _ builder) = (final, base)
  where
    final = builder `mappend` dotEnd

dotArcFromTo :: StateRef -> Arc -> Builder
dotArcFromTo from (Arc label to) =
 mconcat [
   word64Dec from
 , " -> "
 , word64Dec to
 , " [ label=\""
 , correctedLabel
 , "\" ];\n"
 ]
 where
  correctedLabel = if to == 0
                    then "\\$"
                    else char8 (chr (fromIntegral label))

replaceOrRegister :: Compiler (Dotted a)
replaceOrRegister arcs dotted = (stateRef, dotted')
  where
    (stateRef, base') = baseCompiler dotted arcs (base dotted)
    dotted' =  dotted {
      base       = base'
    , dotBuilder = (dotBuilder dotted) `mappend` dotArcs
    , rendered   = HashSet.union (rendered dotted) toBeRendered
    }

    arcSet       = HashSet.fromList (map (\arc -> (stateRef, arc)) arcs)
    toBeRendered = HashSet.difference arcSet (rendered dotted)

    dotArcs = mconcat $ map (dotArcFromTo stateRef . snd) (HashSet.toList toBeRendered)
