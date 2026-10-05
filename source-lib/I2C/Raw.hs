--------------------------------------------------------------------------------
-- | 
-- Module                  : I2C.Raw
-- SPDX-License-Identifier : MIT
-- Copyright               : karamellpelle@hotmail.com
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
--
-- Read and write using 'Storable' types. Note that the implementation of the
-- Storable instance has to be relative to the chip's hardware. For example, many
-- EEPROMs use 2 bytes in big endian for addressing. "I2C.Types" has endian variants 
-- to be used for this purpose.
--------------------------------------------------------------------------------
module I2C.Raw
(
    -- * 
    rawread,
    rawwrite,
    rawmodify,

) where

import Relude 
import Foreign

import I2C.Internal qualified as Internal
import I2C.Chip

----------------------------------------------------------------------------------
-- raw read and write without registers
-- 

-- | Read data of type 'a' from chip.
--   May throw 'I2C.Exception.I2CErr'.
rawread :: forall a t m . (IsChip t, Storable a, MonadIO m) => Internal.Chip t -> m a
rawread chip = liftIO $ 
    Internal.read chip 0 (const $ pure ()) (sizeOf @a undefined) peek
{-# INLINE rawread #-}

-- | Write data of type 'a' to chip.
--   May throw 'I2C.Exception.I2CErr'.
rawwrite :: forall a t m . (IsChip t, Storable a, MonadIO m)  => Internal.Chip t -> a -> m ()
rawwrite chip = \w -> liftIO $
    Internal.write chip (sizeOf w) (flip poke w)
{-# INLINE rawwrite #-}

-- | Modify data of type 'a' on chip.
--   May throw 'I2C.Exception.I2CErr'.
rawmodify :: forall a t m . (IsChip t, Storable a, MonadIO m)  => Internal.Chip t -> (a -> a) -> m a
rawmodify chip = \f -> liftIO $ do
    a <- rawread chip
    let a' = f a
    rawwrite chip a'
    pure a'


