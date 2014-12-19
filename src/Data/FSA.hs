{-# LANGUAGE OverloadedStrings #-}
module Data.FSA
   where

import           Data.FSA.Types
import qualified Data.FSA.Register.Hashed as Hashed
import qualified Data.FSA.Register.Dot as Dot


import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Word (Word16)
import qualified Data.List as List

type ReplaceOrRegister = Compiler Hashed.HashedRegister

type RootArcs = [Arc]

data UncompiledState = UncompiledState {
    ucByte :: !Word16
  , ucArcs :: ![Arc]
  } deriving (Eq, Show)

compileSuffix :: Compiler a
              -> a
              -> [UncompiledState]
              -> (Arc, a)
compileSuffix replaceOrRegister register0 suffix = go register0 suffix
  where
    go _        [] = error "Cannot be called with empty suffix"
    go register ((UncompiledState byte arcs):sx) =
      let
        (arcs', register') = case sx of
          [] -> (arcs, register)
          -- We are really at the end of the word
          _  -> let (a, r) = go register sx in (a:arcs, r)
          -- Traverse till the end
        (ref, register'') = replaceOrRegister arcs' register'
      in (Arc byte ref, register'')

uncompiled :: [Word16]
           -> [UncompiledState]
uncompiled []     = []
uncompiled (w:wx) = (UncompiledState w arcs):(uncompiled wx)
  where
    arcs = if List.null wx then [finalArc] else []

uncompiledBS :: ByteString -> [UncompiledState]
uncompiledBS = uncompiled . map fromIntegral . ByteString.unpack

compile :: Compiler a
        -> [UncompiledState]
        -> [UncompiledState]
        -> a
        -> (RootArcs, [UncompiledState], a)
compile ror new old register = (rootArcs, path, register')
  where
    dummyRoot                    = UncompiledState 0 []
    (root:path, register')       = compile' ror dummyRoot new old register
    (UncompiledState _ rootArcs) = root

compile' :: Compiler a
         -> UncompiledState
         -> [UncompiledState]
         -> [UncompiledState]
         -> a
         -> ([UncompiledState], a)
compile' _  _   []  (_:_) _        = error "words not lexicographically sorted"
compile' _ prev new []    register = (prev:new, register)
  -- New word has whole old as prefix, just path sharing
compile' ror prev new@(n:nx) old@(o:ox) register
  -- New word has some common prefix
  | ucByte n == ucByte o =
    -- If words are equal, recurse
    let
      (path, register') = compile' ror o nx ox register
    in (prev:path, register')
  | otherwise =
    -- If we reached unequal prefixes, compile suffix of old word
    let
      (arc, register') = compileSuffix ror register old
      path = (prev { ucArcs = arc:(ucArcs prev) }):new
    in (path, register')


-- | `mkFST` creates an finite-state automaton from a sorted list of
--   `Word16` codepoints. The compiling behaviour is pluggable.
mkFST :: Compiler a -> a -> [[Word16]] -> (StateRef, a)
mkFST ror register0 wordx = go register0 wordx [] []
  where
    go register [] path rootArcs = (rootRef, register')
      where
        dummyRoot                  = UncompiledState 0 rootArcs
        (Arc _ rootRef, register') = compileSuffix ror register (dummyRoot:path)

    go register (w:wx) path rootArcs = go register' wx path' (rootArcs ++ rootArcs')
      where
        (rootArcs', path', register') = compile ror (uncompiled w) path register

mkFST'BS :: Compiler a -> a -> [ByteString] -> (StateRef, a)
mkFST'BS ror reg bs =
  mkFST ror reg (map (map fromIntegral . ByteString.unpack) bs)

errorStateRef :: StateRef
errorStateRef = 1

finalStateRef :: StateRef
finalStateRef = 0

finalArc :: Arc
finalArc = Arc 0 finalStateRef

mkFST'Test :: [ByteString] -> (StateRef, Dot.Dotted Hashed.HashedRegister)
mkFST'Test = mkFST'BS Dot.replaceOrRegister (Dot.mkDotted Hashed.replaceOrRegister Hashed.empty)


{-
test1'Uncompiled :: [UncompiledState]
test1'Uncompiled = uncompiledBS test1

test1 :: ByteString
test1 = "alex"

test2'Uncompiled :: [UncompiledState]
test2'Uncompiled = uncompiledBS test2

test2 :: ByteString
test2 = "alien"

testBS :: [ByteString]
testBS = ["alex", "alien", "see", "ulrich", "vogel", "zeichen", "ziehen"]
-}

testBS :: [ByteString]
testBS = ["alex", "alien", "see", "ulrich", "vogel", "zeichen", "ziehen"]

