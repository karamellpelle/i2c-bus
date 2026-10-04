--------------------------------------------------------------------------------
-- | 
-- Module                  : I2C.Chip
-- SPDX-License-Identifier : MIT
-- Copyright               : karamellpelle@hotmail.com
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
--------------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes #-}
module I2C.Chip
(
    -- * IsChip
    IsChip (..),
    -- ** Chip addressing
    ChipAddress (..),
    fromChipAddress,


) where

import Relude
import Text.Show qualified
import Numeric (showHex)
import Data.Char (toUpper)


--------------------------------------------------------------------------------
--  chip

-- | Typeclass describing I2C chips
class IsChip t where
    {-# MINIMAL chipName #-}
    -- | Human readable name
    chipName :: Text
    chipName = "(unknown chip)"


--------------------------------------------------------------------------------
--  chip address

-- | A chip's __7 bit__ hardware address on an I2C bus, i.e. the 8 bit R/W addresses shifted down by 1
newtype ChipAddress = ChipAddress Word8
    deriving (Num)

instance Show ChipAddress where
    show (ChipAddress w) = 
        (if 0x10 <= w then "0x" else "0x0") <> fmap toUpper (showHex w "")

-- | Convert from 'ChipAddress'
fromChipAddress :: Num b => ChipAddress -> b
fromChipAddress (ChipAddress addr) = fromIntegral addr
