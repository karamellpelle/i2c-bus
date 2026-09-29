--------------------------------------------------------------------------------
-- | 
-- Module                  : I2C.Chip
-- Description             : Chip type
-- SPDX-License-Identifier : MIT
-- Copyright               : karamellpelle@hotmail.com
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
--------------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes #-}
module I2C.Chip
(
    Chip (..),


) where

import Relude
import Text.Show qualified

import I2C.Types

--------------------------------------------------------------------------------
--  chip

-- | Typeclass for I2C chips
class Chip chip where
    {-# MINIMAL chipName #-}
    -- | Human readable name
    chipName :: Text
    chipName = "(unknown chip)"

