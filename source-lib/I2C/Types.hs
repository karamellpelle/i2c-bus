--------------------------------------------------------------------------------
-- | 
-- Module                  : I2C.Types
-- SPDX-License-Identifier : MIT
-- Copyright               : karamellpelle@hotmail.com
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
--
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module I2C.Types
(
    -- * Storable types 
    --
    -- $storableTypes

    -- ** Little endian
    Word16LE (..),
    Word32LE (..),
    Word64LE (..),
    -- ** Big endian
    Word16BE (..),
    Word32BE (..),
    Word64BE (..),

    -- * Other
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
--  Little and big endian Storable


-- $storableTypes
--
-- These Word variants overrides their 'Storable' instance so that they can
-- be binary serialized as little endian or big endian. This is useful when
-- communicating between host architecture and chip hardware.

newtype Word16LE = Word16LE Word16 
    deriving (Eq, Bits, Num, Integral, Enum, Real, Ord)

instance Storable Word16LE where
    sizeOf w = sizeOf $ un @Word16 w
    alignment w = alignment $ un @Word16 w
#ifdef ARCH_IS_BIG_ENDIAN
    peek = \ptr -> fmap (wrap . byteSwap16) $ peek @Word16 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ byteSwap16 $ un @Word16 w
#else
    peek = \ptr -> fmap wrap $ peek @Word16 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ un @Word16 w
#endif

newtype Word32LE = Word32LE Word32
    deriving (Eq, Bits, Num, Integral, Enum, Real, Ord)

instance Storable Word32LE where
    sizeOf w = sizeOf $ un @Word32 w
    alignment w = alignment $ un @Word32 w
#ifdef ARCH_IS_BIG_ENDIAN
    peek = \ptr -> fmap (wrap . byteSwap32) $ peek @Word32 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ byteSwap32 $ un @Word32 w
#else
    peek = \ptr -> fmap wrap $ peek @Word32 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ un @Word32 w
#endif


newtype Word64LE = Word64LE Word64
    deriving (Eq, Bits, Num, Integral, Enum, Real, Ord)

instance Storable Word64LE where
    sizeOf w = sizeOf $ un @Word64 w
    alignment w = alignment $ un @Word64 w
#ifdef ARCH_IS_BIG_ENDIAN
    peek = \ptr -> fmap (wrap . byteSwap64) $ peek @Word64 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ byteSwap64 $ un @Word64 w
#else
    peek = \ptr -> fmap wrap $ peek @Word64 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ un @Word64 w
#endif

newtype Word16BE = Word16BE Word16
    deriving (Eq, Bits, Num, Integral, Enum, Real, Ord)

instance Storable Word16BE where
    sizeOf w = sizeOf $ un @Word16 w
    alignment w = alignment $ un @Word16 w
#ifdef ARCH_IS_LITTLE_ENDIAN
    peek = \ptr -> fmap (wrap . byteSwap16) $ peek @Word16 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ byteSwap16 $ un @Word16 w
#else
    peek = \ptr -> fmap wrap $ peek @Word16 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ un @Word16 w
#endif

newtype Word32BE = Word32BE Word32
    deriving (Eq, Bits, Num, Integral, Enum, Real, Ord)

instance Storable Word32BE where
    sizeOf w = sizeOf $ un @Word32 w
    alignment w = alignment $ un @Word32 w
#ifdef ARCH_IS_LITTLE_ENDIAN
    peek = \ptr -> fmap (wrap . byteSwap32) $ peek @Word32 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ byteSwap32 $ un @Word32 w
#else
    peek = \ptr -> fmap wrap $ peek @Word32 $ castPtr ptr
    poke = \ptr w -> poke (castPtr ptr) $ un @Word32 w
#endif


newtype Word64BE = Word64BE Word64
    deriving (Eq, Bits, Num, Integral, Enum, Real, Ord)

instance Storable Word64BE where
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

-- | A storable representation of 'a' and 'b' on a chip
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
    -- NOTE: since an I2C chip typically are "continuous bytes", 
    --       I guess aligment is irrelevant for peek and poke here


