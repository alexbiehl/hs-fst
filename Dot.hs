module Main where

import           Control.Applicative
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString8
import           Data.FST
import           Data.FST.Register

main :: IO ()
main = do
  lns <- ByteString8.lines <$> ByteString.getContents
  let (_, dot)= compileListDot (fmap ByteString.unpack lns)
  ByteString.putStr (output dot)
