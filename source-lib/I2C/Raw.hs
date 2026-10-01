--------------------------------------------------------------------------------
-- | 
-- Module                  : I2C.Raw
-- SPDX-License-Identifier : MIT
-- Copyright               : karamellpelle@hotmail.com
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
--
-- Read and write using 'Storable' types. Note that the implementation of the
-- Storable instance has to be relative to the chip hardware. For example, many
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

-- | Read data of type 'a' on chip.
rawread :: forall a chip m . (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> m a
rawread busdev = liftIO $ 
    Internal.read busdev 0 (const $ pure ()) (sizeOf @a undefined) peek

-- | Write data of type 'a' on chip.
rawwrite :: forall a chip m . (Chip chip, Storable a, MonadIO m)  => Internal.BusDevice chip -> a -> m ()
rawwrite busdev = \w -> liftIO $
    Internal.write busdev (sizeOf w) (flip poke w)

-- | Modify data of type 'a' on chip.
rawmodify :: forall a chip m . (Chip chip, Storable a, MonadIO m)  => Internal.BusDevice chip -> (a -> a) -> m a
rawmodify busdev = \f -> liftIO $ do
    a <- rawread busdev
    let a' = f a
    rawwrite busdev a'
    pure a'


