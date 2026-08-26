-- Copyright (c) 2026 karamellpelle@hotmail.com
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy of
-- this software and associated documentation files (the "Software"), to deal in
-- the Software without restriction, including without limitation the rights to
-- use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is furnished to do
-- so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
{-# LANGUAGE CPP #-}
module I2C.Types
(
    ChipAddress (..),
    fromChipAddress,

    RegisterAddress (..),
    fromRegisterAddress,
    
    LE16,
    LE32,
    LE64,
    makeLE16,
    makeLE32,
    makeLE64,
    BE16,
    BE32,
    BE64,
    makeBE16,
    makeBE32,
    makeBE64,
) where

import Relude
import Text.Show qualified

import Numeric (showHex)
import Data.Word
import Foreign.Storable
import Data.Char (toUpper)

--------------------------------------------------------------------------------
--  chip address

-- | hardware's address on bus
newtype ChipAddress = ChipAddress Word8
    deriving (Num)

instance Show ChipAddress where
    show (ChipAddress w) = 
        (if 0x10 <= w then "0x" else "0x0") <> fmap toUpper (showHex w "")

-- | convert from ChipAddress
fromChipAddress :: Num b => ChipAddress -> b
fromChipAddress (ChipAddress addr) = fromIntegral addr


--------------------------------------------------------------------------------
--  registers addressing inside chips
--  our registers are always of size 1 byte 

-- | register address
newtype RegisterAddress = RegisterAddress Word8
    deriving (Num, Storable)

instance Show RegisterAddress where
    show (RegisterAddress w) =
        (if 0x10 <= w then "0x" else "0x0") <> fmap toUpper (showHex w "")

fromRegisterAddress :: Num b => RegisterAddress -> b
fromRegisterAddress (RegisterAddress addr) = fromIntegral addr


--------------------------------------------------------------------------------
--  Little endian Storable

newtype LE16 = LE16 Word16 deriving (Storable)

makeLE16 :: Word16 -> LE16
makeLE16 = 
#ifdef ARCH_IS_BIG_ENDIAN
    LE16 . byteSwap16
#else
    LE16
#endif

newtype LE32 = LE32 Word32 deriving (Storable)

makeLE32 :: Word32 -> LE32
makeLE32 = 
#ifdef ARCH_IS_BIG_ENDIAN
    LE32 . byteSwap32
#else
    LE32
#endif

newtype LE64 = LE64 Word64 deriving (Storable)

makeLE64 :: Word64 -> LE64
makeLE64 = 
#ifdef ARCH_IS_BIG_ENDIAN
    LE64 . byteSwap64
#else
    LE64
#endif

--------------------------------------------------------------------------------
--  Bin endian Storable


newtype BE16 = BE16 Word16 deriving (Storable)

makeBE16 :: Word16 -> BE16
makeBE16 = 
#ifdef ARCH_IS_LITTLE_ENDIAN
    BE16 . byteSwap16
#else
    BE16
#endif

newtype BE32 = BE32 Word32 deriving (Storable)

makeBE32 :: Word32 -> BE32
makeBE32 = 
#ifdef ARCH_IS_LITTLE_ENDIAN
    BE32 . byteSwap32
#else
    BE32
#endif

newtype BE64 = BE64 Word64 deriving (Storable)

makeBE64 :: Word64 -> BE64
makeBE64 = 
#ifdef ARCH_IS_LITTLE_ENDIAN
    BE64 . byteSwap64
#else
    BE64
#endif


