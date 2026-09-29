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
-- This module reexports the backend (picked at compile time). A backend must implement the following:
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
