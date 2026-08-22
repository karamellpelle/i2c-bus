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
module I2C.Types
(
    ChipAddress (..),
    fromChipAddress,

    RegisterAddress (..),
    fromRegisterAddress,

) where

import Relude
import Text.Show qualified

import Numeric (showHex)
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
    deriving (Num)

instance Show RegisterAddress where
    show (RegisterAddress w) =
        (if 0x10 <= w then "0x" else "0x0") <> fmap toUpper (showHex w "")

fromRegisterAddress :: Num b => RegisterAddress -> b
fromRegisterAddress (RegisterAddress addr) = fromIntegral addr

