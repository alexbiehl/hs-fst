module Main where

import           Control.Applicative
import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import           Data.FST
import           Data.Word
import qualified Data.FST.Trie as Trie

setup :: IO [[Word8]]
setup = do
  bx <- ByteString8.lines <$> ByteString.readFile "benchmarks/en_US.txt"
  return (fmap ByteString.unpack bx)

main :: IO ()
main = defaultMain
       [ env setup $ \ ~wordlist ->
          bgroup "creation" [
              bench "packed" $ nf compileList1 wordlist
            , bench "trie" $ nf (Trie.output . snd . compileList2) wordlist
            ]
       ]
