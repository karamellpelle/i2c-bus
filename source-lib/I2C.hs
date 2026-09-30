--------------------------------------------------------------------------------
-- | 
-- Module                  : I2C
-- Description             : Main module
-- SPDX-License-Identifier : MIT
-- Copyright               : karamellpelle@hotmail.com
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
-- 
-- Main module for communication on the I2C bus. Importing this module 
-- will typically give you all that you need.
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
--------------------------------------------------------------------------------
module I2C
(

    -- * Chip connection
    Internal.BusDevice,
    openChip,
    closeChip,

    -- * Primitives
    module I2C.Types,
    module I2C.Chip,
    -- * Read and write Storable
    module I2C.Raw,
    -- * Registers utilities
    module I2C.Register,
    -- * Template Haskell
    module I2C.TH,
    -- * Exception
    I2CErr (..),
) where

import Relude
import I2C.Types
import I2C.Exception
import I2C.Chip
import I2C.Internal qualified as Internal
import I2C.Raw
import I2C.Register
import I2C.TH


--------------------------------------------------------------------------------
--  reexport functions as lifted IO

-- | Open a connection to a chip of type 'chip' based on bus identifier and hardware address on bus.
--   The bus identifier on Linux is typically something like @\/dev\/i2c-N@.
--   May throw 'I2CErr'.
openChip :: forall chip m . (Chip chip, MonadIO m) => 
            Text ->                       -- ^ Bus identifier
            ChipAddress ->                -- ^ /7 bit/ hardware address
            m (Internal.BusDevice chip)
openChip id addr = liftIO $ Internal.openChip id addr


-- | Close connection to chip. 
--   Shall not throw 'I2CErr'.
closeChip :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> m ()
closeChip busdev = liftIO $ Internal.closeChip busdev
