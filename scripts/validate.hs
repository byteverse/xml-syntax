{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified System.IO as IO
import qualified Xml

main :: IO ()
main = do
  contents <- Chunks.hGetContents IO.stdin
  case Xml.decode (Chunks.concat contents) of
    Nothing -> fail "Failed to decode"
    Just x -> print x
