{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.FST where

import           Data.FST.Types
import qualified Data.FST.Register as Register
import qualified Data.FST.Register.Dot as Dot

import           Data.ByteString
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import           Data.Word (Word8, Word32)

type RootArcs = [Arc]

uncompile :: [Word8] -> a -> [UncompiledState a]
uncompile []     _ = []
uncompile (w:wx) a = UncompiledState w arcs output : uncompile wx a
  where
    output   = if List.null wx then Just a else Nothing
    arcs     = if List.null wx then [Register.finalArc] else []

compileSuffix :: ReplaceOrRegister s a
              -> [UncompiledState a]
              -> s
              -> (Arc, s)
compileSuffix replaceOrRegister = go
  where
    go [] _ = error "compileSuffix: empty nodes"
    go (UncompiledState byte arcs output : sx) register =
      let
        (arcs', register') = case sx of
          -- We are really at the end of the word
          [] -> (arcs, register)
          -- Traverse till the end
          _  -> let (a, r) = go sx register in (a:arcs, r)
        node = UncompiledState byte arcs' output
      in replaceOrRegister node register'

compile :: ReplaceOrRegister s a
        -> [UncompiledState a]
        -> [UncompiledState a]
        -> s
        -> (RootArcs, [UncompiledState a], s)
compile ror new old register = (rootArcs, path, register')
  where
    dummyRoot                      = UncompiledState 0 [] Nothing
    (root:path, register')         = compile' ror dummyRoot new old register
    (UncompiledState _ rootArcs _) = root

compile' :: ReplaceOrRegister s a
         -> UncompiledState a
         -> [UncompiledState a]
         -> [UncompiledState a]
         -> s
         -> ([UncompiledState a], s)
compile' _  _   []  (_:_) _        = error "compile: No lexicographic order"
compile' _ prev new []    register = (prev:new, register)
  -- New word has whole old as prefix, just path sharing
compile' ror prev new@(n:nx) old@(o:ox) register
  -- New word has some common prefix
  | ucLabel n == ucLabel o =
    -- If words are equal, recurse
    let
      (path, register') = compile' ror o nx ox register
    in (prev:path, register')
  | otherwise =
    -- If we reached unequal prefixes, compile suffix of old word
    let
      (arc, register') = compileSuffix ror old register
      path = (prev { ucArcs = arc:ucArcs prev }):new
    in (path, register')

-- | `mkFST` creates an finite-state automaton from a sorted list of
--   `Word16` codepoints. The compiling behaviour is pluggable.
mkFST :: ReplaceOrRegister s a -> s -> [([Word8], a)] -> (StateRef, s)
mkFST ror register0 wordx = go register0 wordx [] []
  where
    go register [] path rootArcs = (rootRef, register')
      where
        dummyRoot                     = UncompiledState 0 rootArcs Nothing
        (Arc _ _ !rootRef, !register') = compileSuffix ror (dummyRoot:path) register

    go register ((w, a):wx) path rootArcs = go register' wx path' (rootArcs' ++ rootArcs)
      where
        (!rootArcs', !path', !register') = compile ror (uncompile w a) path register

mkFST'BS :: ReplaceOrRegister s () -> s -> [ByteString] -> (StateRef, s)
mkFST'BS ror reg =
  mkFST ror reg . fmap (\bs -> (bs, ())) . fmap ByteString.unpack

testBS :: [ByteString]
testBS = ["alex", "alien", "see", "ulrich", "vogel", "zeichen", "ziehen"]

dotRegister = Dot.empty Register.empty Register.replaceOrRegister

dotAutomaton = Dot.showDot $ snd $ mkFST'BS Dot.dotted dotRegister testBS

test = print dotAutomaton