module Main where

import System.Environment
import System.IO.Posix.MMap

import Data.ByteString.CountByte

main :: IO ()
main = do
  [path] <- getArgs
  contents <- unsafeMMapFile path
  print $ countByte contents 10
