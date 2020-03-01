{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}
{-# LANGUAGE Strict #-}

module Data.ByteString.CountByte.Asm.SSE42(countChars) where

import qualified Data.ByteString as BS
import Foreign.Ptr
import GHC.Word

import Language.Asm.Inline
import Language.Asm.Inline.QQ
import Language.Asm.Inline.Util

defineAsmFun "countCharsSSE42"
  [asmTy| (ch : Word8) (ptr : Ptr Word8) (len : Int) | (cnt : Int) |] $
  unroll "i" [12..15]
  [asm|
  push %r{i}|] <> [asm|
  vmovd {ch}, %xmm15
  vpxor %xmm0, %xmm0, %xmm0
  vpshufb %xmm0, %xmm15, %xmm15

  shr $7, {len}

  mov $16, %eax
  mov $16, %edx

  xor {cnt}, {cnt}

  {move ptr rdi}
loop: |] <> unrolls "i" [1..8] [
  [asm|
  vmovdqa {(i - 1) * 0x10}({ptr}), %xmm{i}
  |], [asm|
  vpcmpestrm $10, %xmm15, %xmm{i}
  vmovq %xmm0, %r{i + 7}
  popcnt %r{i + 7}, %r{i + 7}
  |], [asm|
  add %r{i + 7}, {cnt}
  |]
  ] <>
  [asm|
  add $128, {ptr}
  dec {len}
  jnz loop|] <> unroll "i" [15,14..12] [asm|
  pop %r{i} |]

countChars :: Word8 -> BS.ByteString -> Int
countChars ch bs | BS.length bs <= 256 = BS.count ch bs
                 | otherwise = BS.count ch (substr 0 startLen bs)
                             + countCharsSSE42 ch (castPtr alignedPtr) alignedLen
                             + BS.count ch (substr endPos endLen bs)
  where
    basePtr = getBSAddr bs
    alignedPtr = alignPtr basePtr alignment
    startLen = alignedPtr `minusPtr` basePtr
    (alignedLen, endLen) = let remLen = BS.length bs - startLen
                               remainder = remLen `rem` alignment
                            in (remLen - remainder, remainder)
    endPos = startLen + alignedLen
    alignment = 128

substr :: Int -> Int -> BS.ByteString -> BS.ByteString
substr start len = BS.take len . BS.drop start
