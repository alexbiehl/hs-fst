{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString8
import           Data.FST
import           Data.FST.Register
import qualified Data.FST.Trie.Lookup as Lookup
import           Data.FST.Trie (Trie(..))

main :: IO ()
main = do
  lns <- ByteString8.lines <$> ByteString.getContents
  let (root, triereg) = compileListTrie (fmap ByteString.unpack lns)
      (Trie trie)     = output triereg

  putStrLn $ "root:" ++ show root

  ByteString.writeFile "trie.out" (ByteString.fromStrict trie)

  forM_ lns $ \l -> do
    let hash = Lookup.lookup 0 (Trie trie) (ByteString.toStrict l)
    ByteString.putStr l
    ByteString.putStr " "
    putStrLn (show hash)
