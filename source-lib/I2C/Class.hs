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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
module I2C.Class
(
    ChipAddress (..),
    RegisterAddress (..),
    fromChipAddress,
    fromRegisterAddress,

    Chip (..),
    Register1 (..),
    Register2 (..),

) where

import Relude
import I2C.Exception
import Text.Show qualified

import Numeric (showHex)
import Data.Char (toUpper)

--------------------------------------------------------------------------------
--  chips

-- | hardware's address on bus
newtype ChipAddress = ChipAddress Word8
    deriving (Num)

instance Show ChipAddress where
    show (ChipAddress addr) = showAddress addr

fromChipAddress :: Num b => ChipAddress -> b
fromChipAddress (ChipAddress addr) = fromIntegral addr


-- | typeclass for I2C chips
class Chip a where
    -- | _7 bit_ address on I2C bus, i.e. the 8 bit R/W addresses shifted down by 1
    chipAddress :: ChipAddress
    -- | human readable identifier
    chipName :: Text
    chipName = "(unknown)"


--------------------------------------------------------------------------------
--  registers

-- | register address
newtype RegisterAddress = RegisterAddress Word8
    deriving (Num)

instance Show RegisterAddress where
    show (RegisterAddress addr) = showAddress addr

fromRegisterAddress :: Num b => RegisterAddress -> b
fromRegisterAddress (RegisterAddress addr) = fromIntegral addr


-- | typeclass for a chip's register containing 1 Word8.
class Register1 reg where
    -- | register index inside Chip
    register1Address :: RegisterAddress
    -- | human readable name
    register1Name :: Text
    register1Name = show $ register1Address @reg
  
-- | typeclass for a chip's register containing 1 Word16.
class Register2 reg where
    -- | register index inside Chip
    register2Address :: RegisterAddress
    -- | human readable name
    register2Name :: Text
    register2Name = show $ register2Address @reg
  

--------------------------------------------------------------------------------
--  

showWord8 :: Word8 -> String
showWord8 w | 0x10 <= w   = fmap toUpper $ showHex w ""
            | otherwise   = fmap toUpper $ "0" <> showHex w ""

showAddress :: Word8 -> String
showAddress = mappend "0x" . showWord8

