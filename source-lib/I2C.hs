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

-- | 
-- Module                  : I2C
-- Description             : Main module
-- SPDX-License-Identifier : MIT
-- Copyright               : (c) karamellpelle@hotmail.com, 2026
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
-- 
-- Main module for communication on the I2C bus. It will typically 
-- give you all you need.
--
-- Example: 
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import I2C
-- >
-- > $(chip "PCF8575")
-- > 
-- > testPCF8575 :: IO ()
-- > testPCF8575 = do
-- >     busdev <- openChip @PCF8575 "/dev/i2c-1" 0x20
-- >     
-- >     forM_ [0..0x00FF] $ \ix -> do
-- >         rawwrite @Store16LE busdev ix
-- >         threadDelay 400000
--
module I2C
(
    Chip (..),
    BusDevice,

    -- * Chip connection
    openChip,
    closeChip,

    -- * Primitives
    module I2C.Types,
    -- * Raw communication using 'Foreign.Storable'
    module I2C.Raw,
    -- * Registers utilities
    module I2C.Register,
    -- * Template Haskell
    module I2C.TH,
    -- * Exception
    I2CErr (..),
) where

import I2C.Types
import I2C.Exception
import I2C.Chip
import I2C.Internal
import I2C.Raw
import I2C.Register
import I2C.TH
