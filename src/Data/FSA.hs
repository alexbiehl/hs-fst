{-# LANGUAGE OverloadedStrings #-}
module Data.FSA where

import           Data.FSA.Types
import           Data.FSA.Register.Hashed

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Word (Word16, Word64)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable
import qualified Data.List as List
import           Data.Monoid
import           Data.Function (on)
import           Data.Char (chr, ord)

type ReplaceOrRegister = Compiler HashedRegister

data UncompiledState = UncompiledState {
    ucByte :: !Word16
  , ucArcs :: ![Arc]
  } deriving (Eq, Show)

test1'Uncompiled :: [UncompiledState]
test1'Uncompiled = uncompiledBS test1

test1 :: ByteString
test1 = "alex"

test2'Uncompiled :: [UncompiledState]
test2'Uncompiled = uncompiledBS test2

test2 :: ByteString
test2 = "alien"

compileSuffix :: Compiler a
              -> a
              -> [UncompiledState]
              -> (Arc, a)
compileSuffix replaceOrRegister register sx = go register sx
  where
    go register ((UncompiledState byte arcs):sx) =
      let
        (arcs', register') = case sx of
          [] -> (arcs, register)
          -- ^ We are really at the end of the word
          _  -> let (a, r) = go register sx in (a:arcs, r)
          -- ^ Traverse till the end
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
compile' _ prev new [] register = (prev:new, register)
  -- ^ New word has whole old as prefix, just path sharing

compile' ror prev new@(n:nx) old@(o:ox) register
  -- ^ New word has some common prefix
  | ucByte n == ucByte o =
    -- ^ If words are equal, recurse
    let
      (path, register') = compile' ror o nx ox register
    in (prev:path, register')
  | otherwise =
    -- ^ If we reached unequal prefixes, compile suffix of old word
    let
      (arc, register') = compileSuffix ror register old
      path = (prev { ucArcs = arc:(ucArcs prev) }):new
    in (path, register')

mkFST :: Compiler a -> a -> [[Word16]] -> a
mkFST ror register wordx = go register wordx [] []
  where
    go register []     _    rootArcs = register'
      where
        (ref, register') = ror rootArcs register

    go register (w:wx) path rootArcs = go register' wx path' (rootArcs ++ rootArcs')
      where
        (rootArcs', path', register') = compile ror (uncompiled w) path register

mkFST'BS :: Compiler a -> a -> [ByteString] -> a
mkFST'BS ror reg bs =
  mkFST ror reg (map (map fromIntegral . ByteString.unpack) bs)

errorStateRef :: StateRef
errorStateRef = 1

finalStateRef :: StateRef
finalStateRef = 0

finalArc :: Arc
finalArc = Arc 0 finalStateRef

mkFST'Test :: [ByteString] -> HashedRegister
mkFST'Test = mkFST'BS replaceOrRegister empty

testBS :: [ByteString]
testBS = ["alex", "alien", "see", "ulrich", "vogel", "zeichen", "ziehen"]

