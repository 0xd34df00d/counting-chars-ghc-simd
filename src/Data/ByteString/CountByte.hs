{-# LANGUAGE Strict, MagicHash #-}
{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.CountByte
( countByte
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import GHC.ForeignPtr
import GHC.Prim
import GHC.Types
import GHC.Word

countByte :: BS.ByteString -> Word8 -> Int
countByte (BS.toForeignPtr -> (ForeignPtr addr _, I# offset, I# len)) (W8# w) = I# (countByte' addr' len w)
  where addr' = plusAddr# addr offset

countByte' :: Addr# -> Int# -> Word# -> Int#
countByte' addr len w = countPrefix addr len w +# countCacheline addr len w

countPrefix :: Addr# -> Int# -> Word# -> Int#
countPrefix addr len w = go 0# 0#
  where
    go pos counter
      | 0# <- pos ==# len
      , 0# <- addrRem ==# 0# = go (pos +# 1#) (counter +# (indexWord8OffAddr# addr pos `eqWord#` w))
      | otherwise = counter
      where addrRem = (addr `plusAddr#` pos) `remAddr#` 64#

countCacheline :: Addr# -> Int# -> Word# -> Int#
countCacheline addr len w = go (64# -# (addr `remAddr#` 64#)) 0#
  where
    go :: Int# -> Int# -> Int#
    go pos counter
      | 1# <- pos <=# end = undefined
      | otherwise = counter
    pat = broadcastWord8X64# w
    end = len -# 64#
