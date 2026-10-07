--------------------------------------------------------------------------------
-- | 
-- Module                  : I2C.Internal
-- SPDX-License-Identifier : MIT
-- Copyright               : karamellpelle@hotmail.com
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module I2C.Internal
(
    -- $info

    -- * Re-exported backend
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
-- This module reexports the backend picked at compile time. A backend must implement the following:
--
--  > data Chip t
--  > openChip  :: (IsChip t) => Text -> ChipAddress -> IO (Chip t)
--  > closeChip :: (IsChip t) => Chip t -> IO ()
--  > write     :: (IsChip t) => Chip t -> Int -> (Ptr w -> IO ()) -> IO ()
--  > read      :: (IsChip t) => Chip t -> Int -> (Ptr w -> IO ()) -> Int -> (Ptr r -> IO r) -> IO r
--  > writeSome :: (IsChip t) => Chip t -> Int -> (Ptr w -> IO ()) -> IO Int
--  > readSome  :: (IsChip t) => Chip t -> Int -> (Ptr w -> IO ()) -> Int -> (Int -> Ptr r -> IO r)-> IO r
--
-- See the documentation in "I2C.Internal.Linux" for more information on how a backend 
-- implementation should behave.
