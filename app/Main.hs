module Main where

import Control.Exception
import System.Environment
import System.IO.Posix.MMap
import System.TimeIt

import Data.ByteString.CountByte.Asm.SSE42

main :: IO ()
main = do
  [path] <- getArgs
  contents <- unsafeMMapFile path
  (time, counts) <- timeItUserspaceT $ evaluate $ sum $ map (`countChars` contents) (replicate 10 32)
  print $ counts `div` 10
  print $ time * 1000 / 10
