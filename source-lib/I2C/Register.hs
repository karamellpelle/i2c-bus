--------------------------------------------------------------------------------
-- | 
-- Module                  : I2C.Register
-- SPDX-License-Identifier : MIT
-- Copyright               : karamellpelle@hotmail.com
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
--
-- Utilities for working with registers in chips
--------------------------------------------------------------------------------
module I2C.Register
(
    -- * Register
    Register (..),
    -- ** Register addressing
    RegisterAddress (..),
    fromRegisterAddress,

    -- * Read and write registers
    regread,
    regwrite,
    regmodify,

    -- * Read and write registers (direct addressing)
    regread',
    regwrite',
    regmodify',

) where

import Relude
import Data.Default
import Text.Show qualified
import Numeric (showHex)
import Data.Char (toUpper)
import Foreign

import I2C.Internal qualified as Internal
import I2C.Chip
import I2C.Types


--------------------------------------------------------------------------------
--  Register

-- | Register addressing inside chips.
--   Register addresses are always of size 1 byte.
newtype RegisterAddress = RegisterAddress Word8
    deriving (Num, Storable)

instance Show RegisterAddress where
    show (RegisterAddress w) =
        (if 0x10 <= w then "0x" else "0x0") <> fmap toUpper (showHex w "")

-- | Convert from 'RegisterAddress'
fromRegisterAddress :: Num b => RegisterAddress -> b
fromRegisterAddress (RegisterAddress addr) = fromIntegral addr

-- | Index into register of type 't' of a chip of type 'chip'
data Register chip t = Register Text RegisterAddress


--------------------------------------------------------------------------------
--  using Register

-- | Read register
regread :: (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> Register chip a -> m a
regread busdev (Register _name addr) = 
    regread' busdev addr

-- | Write register
regwrite :: (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> Register chip a -> a -> m ()
regwrite busdev (Register _name addr) = 
    regwrite' busdev addr

-- | Modify register
regmodify :: (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> Register chip a -> (a -> a) -> m a
regmodify = \busdev reg f -> do
    a <- regread busdev reg
    let a' = f a
    regwrite busdev reg a'
    pure a'


--------------------------------------------------------------------------------
--  raw addressing, no Register

-- | Read register at address
regread' :: forall a chip m . (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> m a
regread' busdev addr = 
    liftIO $ Internal.read busdev (sizeOf addr) (flip poke addr) (sizeOf @a undefined) peek

-- | Write register at address
regwrite' :: forall a chip m . (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> a -> m ()
regwrite' busdev addr = \a -> do
    let w = StorableAB addr a
    liftIO $ Internal.write busdev (sizeOf w) (flip poke w) 

-- | Modify register at address
regmodify' :: forall a chip m . (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> (a -> a) -> m a
regmodify' = \busdev addr f -> do
    a <- regread' busdev addr
    let a' = f a
    regwrite' busdev addr a'
    pure a'
