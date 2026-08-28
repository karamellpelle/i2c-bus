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
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABStoreLE FOR ANY CLAIM, DAMAGES OR OTHER
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
    
    Store8 (..),
    Store16LE (..),
    Store32LE (..),
    Store64LE (..),
    Store16BE (..),
    Store32BE (..),
    Store64BE (..),
    StorableAB (..),
) where

import Relude
import Relude.Extra.Newtype
import Text.Show qualified

import Numeric (showHex)
import Data.Word
import Foreign
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
--  Little and big endian Storable

-- consequent behaviour is good for the TH module
newtype Store8 = Store8 Word8 deriving (Storable)

newtype Store16LE = Store16LE Word16

instance Storable Store16LE where
    sizeOf w = sizeOf $ un @Word16 w
    alignment w = alignment $ un @Word16 w
#ifdef ARCH_IS_BIG_ENDIAN
    peek = \ptr -> fmap (wrap . byteSwap16) $ peek @Word16 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ byteSwap16 $ un @Word16 w
#else
    peek = \ptr -> fmap wrap $ peek @Word16 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ un @Word16 w
#endif

newtype Store32LE = Store32LE Word32

instance Storable Store32LE where
    sizeOf w = sizeOf $ un @Word32 w
    alignment w = alignment $ un @Word32 w
#ifdef ARCH_IS_BIG_ENDIAN
    peek = \ptr -> fmap (wrap . byteSwap32) $ peek @Word32 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ byteSwap32 $ un @Word32 w
#else
    peek = \ptr -> fmap wrap $ peek @Word32 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ un @Word32 w
#endif


newtype Store64LE = Store64LE Word64

instance Storable Store64LE where
    sizeOf w = sizeOf $ un @Word64 w
    alignment w = alignment $ un @Word64 w
#ifdef ARCH_IS_BIG_ENDIAN
    peek = \ptr -> fmap (wrap . byteSwap64) $ peek @Word64 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ byteSwap64 $ un @Word64 w
#else
    peek = \ptr -> fmap wrap $ peek @Word64 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ un @Word64 w
#endif

newtype Store16BE = Store16BE Word16

instance Storable Store16BE where
    sizeOf w = sizeOf $ un @Word16 w
    alignment w = alignment $ un @Word16 w
#ifdef ARCH_IS_LITTLE_ENDIAN
    peek = \ptr -> fmap (wrap . byteSwap16) $ peek @Word16 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ byteSwap16 $ un @Word16 w
#else
    peek = \ptr -> fmap wrap $ peek @Word16 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ un @Word16 w
#endif

newtype Store32BE = Store32BE Word32

instance Storable Store32BE where
    sizeOf w = sizeOf $ un @Word32 w
    alignment w = alignment $ un @Word32 w
#ifdef ARCH_IS_LITTLE_ENDIAN
    peek = \ptr -> fmap (wrap . byteSwap32) $ peek @Word32 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ byteSwap32 $ un @Word32 w
#else
    peek = \ptr -> fmap wrap $ peek @Word32 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ un @Word32 w
#endif


newtype Store64BE = Store64BE Word64

instance Storable Store64BE where
    sizeOf w = sizeOf $ un @Word64 w
    alignment w = alignment $ un @Word64 w
#ifdef ARCH_IS_LITTLE_ENDIAN
    peek = \ptr -> fmap (wrap . byteSwap64) $ peek @Word64 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ byteSwap64 $ un @Word64 w
#else
    peek = \ptr -> fmap wrap $ peek @Word64 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ un @Word64 w
#endif


--------------------------------------------------------------------------------
--  Storable pair

-- | a storable representation _on the I2C chip_ of 'a' and 'b'
data StorableAB a b = 
    StorableAB !a !b


instance (Storable a, Storable b) => Storable (StorableAB a b) where
    sizeOf (StorableAB a b) = sizeOf a + sizeOf b  
    alignment (StorableAB a b) = lcm (alignment a) (alignment b)
    peek = \ptr -> do
        a <- peek $ plusPtr ptr 0
        b <- peek $ plusPtr ptr $ sizeOf a
        pure $ StorableAB a b
    poke = \ptr (StorableAB a b) -> do
        poke (plusPtr ptr 0) $ a
        poke (plusPtr ptr $ sizeOf a) $ b
    -- NOTE: since an I2C chip typically uses "continuous bytes", 
    --       I guess aligment is irrelevant for peek and poke below


