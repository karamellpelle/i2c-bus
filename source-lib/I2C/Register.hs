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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module I2C.Register
(
    Register (..),

    regread1,
    regwrite1,
    regmodify1,
    regread2,
    regwrite2,
    regmodify2,

) where

import Relude
import Data.Default
import Text.Show qualified
import Foreign

import I2C.Internal qualified as Internal
import I2C.Chip
import I2C.Types


--------------------------------------------------------------------------------
--  class Register
--

-- | typeclass for a chip's register. 
class Register reg where
    -- | register index inside Chip
    registerAddress :: RegisterAddress
    -- | human readable name
    registerName :: Text
    registerName = "unknown@" <> (show $ registerAddress @reg)

    -- | type of content in register
    type RegisterItem reg

    -- | bus functions
    regread   :: (Chip chip, MonadIO m) => Internal.BusDevice chip -> m reg
    regwrite  :: (Chip chip, Default reg, MonadIO m) => Internal.BusDevice chip -> (reg -> reg) -> m reg
    regmodify :: (Chip chip, MonadIO m) => Internal.BusDevice chip -> (reg -> reg) -> m reg


--------------------------------------------------------------------------------
--  Word8 (register + Word8)

-- | read 1 raw byte from register at chip
regread1 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> m Word8
regread1 busdev = \raddr -> liftIO $ 
    Internal.read busdev raddr 
         
-- | write raw byte to register at chip. returns value written
regwrite1 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> Word8 -> m ()
regwrite1 busdev = \addr w -> liftIO $ 
    Internal.write busdev $ StorableAB addr w 

-- | modify current raw content of register
regmodify1 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> (Word8 -> Word8) -> m Word8
regmodify1 busdev = \addr f -> liftIO $ do
    w <- regread1 busdev addr
    let w' = f w
    regwrite1 busdev addr w'
    pure w'

--------------------------------------------------------------------------------
--  Word16 (register + Word16)

-- | read 2 raw bytes from register
regread2 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> m Word16
regread2 busdev = \addr -> liftIO $ 
    --Internal.readRegData2 busdev addr 
    undefined

-- | write 2 raw bytes to register. 
regwrite2 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> Word16 -> m ()
regwrite2 busdev = \addr w -> liftIO $ 
    --Internal.writeRegData2 busdev addr w 
    undefined

-- | modify current content of register
regmodify2 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> (Word16 -> Word16) -> m Word16
regmodify2 busdev = \addr f -> liftIO $ do
    --w <- regread2 @chip busdev addr
    --let w' = f w
    --regwrite2 busdev addr w'
    --pure w'
    undefined




-- | read 1 raw byte from register at chip
regreadStorable :: forall chip a m . (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> m a
regreadStorable busdev = \raddr -> liftIO $ 
    Internal.read busdev raddr 
         
-- | write raw byte to register at chip. returns value written
regwriteStorable :: forall chip a m . (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> a -> m ()
regwriteStorable busdev = \addr w -> liftIO $ 
    Internal.write busdev $ StorableAB addr w 

-- | modify current raw content of register
regmodifyStorable :: forall chip a m . (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> (a -> a) -> m a
regmodifyStorable busdev = \addr f -> liftIO $ do
    w <- regreadStorable busdev addr
    let w' = f w
    regwriteStorable busdev addr w'
    pure w'

--------------------------------------------------------------------------------
--  

-- | a storable representation _on the I2C chip_ of 'a' and 'b'
data StorableAB a b = 
    StorableAB !a !b


-- NOTE: since this is a I2C chip with "continuous bytes", 
--       I guess aligment is irrelevant for peek and poke
instance (Storable a, Storable b) => Storable (StorableAB a b) where
    sizeOf (StorableAB a b) = sizeOf a + sizeOf b  
    alignment (StorableAB a b) = lcm (alignment a) (alignment b)
    peek = \ptr -> do
        a <- peek $ plusPtr ptr 0
        b <- peek $ plusPtr ptr $ sizeOf a
        pure $ StorableAB a b
    poke = \ptr (StorableAB a b) -> do
        poke (plusPtr ptr 0) $ a
        poke (plusPtr ptr $ sizeOf a) $ b


