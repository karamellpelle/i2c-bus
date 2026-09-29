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
--------------------------------------------------------------------------------
-- | 
-- Module                  : I2C.Internal
-- Description             : Backend
-- SPDX-License-Identifier : MIT
-- Copyright               : karamellpelle@hotmail.com
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module I2C.Internal
(
    -- $info

    -- * Reexported backend
    --
#ifdef INTERNAL_USE_LINUX
    module I2C.Internal.Linux,
#endif

) where

import Relude 

#ifdef INTERNAL_USE_LINUX
import I2C.Internal.Linux
#endif

-- $info
--
-- This module reexports the backend. A backend must implement the following:
--
--  > data BusDevice chip
--  > openChip :: (Chip chip) => Text -> ChipAddress -> IO (BusDevice chip)
--  > closeChip :: (Chip chip) => BusDevice chip -> IO ()
--  > write :: (Chip chip) => BusDevice chip -> Int -> (Ptr w -> IO ()) -> IO ()
--  > read :: (Chip chip)  => BusDevice chip -> Int -> (Ptr w -> IO ()) -> Int -> (Ptr r -> IO r) -> IO r
--  > writeSome :: (Chip chip) => BusDevice chip -> Int -> (Ptr w -> IO ()) -> IO Int
--  > readSome :: (Chip chip) => BusDevice chip -> Int -> (Ptr w -> IO ()) -> Int -> (Int -> Ptr r -> IO r)-> IO r
--
-- See the documentation in "I2C.Internal.Linux" for more information on how a backend 
-- implementation should behave.
