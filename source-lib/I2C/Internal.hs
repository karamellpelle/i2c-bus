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
{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
module I2C.Internal
(
    module I2C.Chip,

    BusDevice,
    openChip,
    closeChip,

    Register (..),

    rawread,
    rawwrite,
    rawmodify,

    smbusread0,
    smbuswrite0,
    smbusmodify0,
    smbusread1,
    smbuswrite1,
    smbusmodify1,
    smbusread2,
    smbuswrite2,
    smbusmodify2,

    regread1,
    regread2,
    regwrite1,
    regwrite2,
    regmodify1,
    regmodify2,

) where

import Relude hiding (modify)
import Data.Default
import Foreign

import I2C.Chip

#ifdef I2C_INTERNAL_LINUX
import I2C.Internal.Linux
#endif
#ifdef I2C_INTERNAL_EMPTY
import I2C.Internal.Empty
#endif


----------------------------------------------------------------------------------
-- raw read and write to chip without registers (SMBus free). size determined by 
-- 'Storable a'

rawread :: forall chip a m . (Chip chip, Storable a, MonadIO m) => BusDevice chip -> m a
rawread busdev = liftIO $
    readRaw @chip busdev

rawwrite :: forall chip a m . (Chip chip, Storable a, MonadIO m)  => BusDevice chip -> a -> m ()
rawwrite busdev = \a -> liftIO $
    writeRaw @chip busdev a

rawmodify :: forall chip a m . (Chip chip, Storable a, MonadIO m)  => BusDevice chip -> (a -> a) -> m a
rawmodify busdev = \f -> liftIO $ do
    a <- rawread @chip busdev
    let a' = f a
    rawwrite @chip busdev a'
    pure a'


--------------------------------------------------------------------------------
--  class Register
--

-- | typeclass for a chip's register. 
class Register reg where
    -- | register index inside Chip
    registerAddress :: RegisterAddress
    -- | human readable name
    registerName :: Text
    registerName = show $ registerAddress @reg

    -- | bus functions
    regread   :: (Chip chip, MonadIO m) => BusDevice chip -> m reg
    regwrite  :: (Chip chip, MonadIO m, Default reg) => BusDevice chip -> (reg -> reg) -> m reg
    regmodify :: (Chip chip, MonadIO m) => BusDevice chip -> (reg -> reg) -> m reg



--------------------------------------------------------------------------------
--  Word8 (smbus "byte")

---- | read single Word8 from chip (no register)
smbusread0 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> m Word8
smbusread0 busdev = liftIO $ 
    readData0 busdev 

-- | write single Word8 to chip (no register)
smbuswrite0 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> Word8 -> m ()
smbuswrite0 busdev = \w -> liftIO $ 
    writeData0 busdev w

-- | modify current single Word8 in chip (no register)
smbusmodify0 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> (Word8 -> Word8) -> m Word8
smbusmodify0 busdev = \f -> liftIO $ do
    w <- smbusread0 @chip busdev
    let w' = f w
    smbuswrite0 @chip busdev $ f w'
    pure w' 


--------------------------------------------------------------------------------
--  Word8 (register + smbus "byte")

-- | read 1 raw byte from register at chip
smbusread1 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> m Word8
smbusread1 busdev = \raddr -> liftIO $ 
    readData1 busdev raddr
         
-- | write raw byte to register at chip. returns value written
smbuswrite1 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> Word8 -> m ()
smbuswrite1 busdev = \addr w -> liftIO $ 
    writeData1 busdev addr w 

-- | modify current raw content of register
smbusmodify1 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> (Word8 -> Word8) -> m Word8
smbusmodify1 busdev = \addr f -> liftIO $ do
    w <- smbusread1 @chip busdev addr
    let w' = f w
    smbuswrite1 @chip busdev addr w'
    pure w'
    
--------------------------------------------------------------------------------
--  Word16 (register + smbus "word")

-- | read 2 raw bytes from register
smbusread2 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> m Word16
smbusread2 busdev = \addr -> liftIO $ 
    readData2 busdev addr 

-- | write 2 raw bytes to register. 
smbuswrite2 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> Word16 -> m ()
smbuswrite2 busdev = \addr w -> liftIO $ 
    writeData2 busdev addr w 

-- | modify current content of register
smbusmodify2 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> (Word16 -> Word16) -> m Word16
smbusmodify2 busdev = \addr f -> liftIO $ do
    w <- smbusread2 @chip busdev addr
    let w' = f w
    smbuswrite2 busdev addr w'
    pure w'

--------------------------------------------------------------------------------
--  TODO: smbusxxxN (register + smbus "block")
--        1 byte length + n bytes data). according to spec, n is not allowed to 
--        be 0 for some reason, https://smbus.org/specs/smbus20.pdf#%5B%7B%22num%22%3A222%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22FitB%22%7D%5D


--------------------------------------------------------------------------------
--  helpers intented for Register instancing

regread1 :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, MonadIO m) => 
            BusDevice chip -> m reg
regread1 busdev = 
    fmap coerce $ smbusread1 @chip busdev (registerAddress @reg)

regwrite1 :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, Default reg, MonadIO m) => 
            BusDevice chip -> (reg -> reg) -> m reg
regwrite1 busdev = \f -> do
    let r' = f def
    smbuswrite1 @chip busdev (registerAddress @reg) (coerce $ r')
    pure r'

regmodify1 :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, MonadIO m) => 
            BusDevice chip -> (reg -> reg) -> m reg
regmodify1 busdev = \f -> do
    r <- regread1 busdev
    let r' = f r
    smbuswrite1 busdev (registerAddress @reg) $ coerce $ r'
    pure r'

     
regread2 :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, MonadIO m) => 
            BusDevice chip -> m reg
regread2 busdev =
    fmap coerce $ smbusread2 @chip busdev (registerAddress @reg)

regwrite2 :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, Default reg, MonadIO m) => 
            BusDevice chip -> (reg -> reg) -> m reg
regwrite2 busdev = \f -> do
    let r' = f def
    smbuswrite2 @chip busdev (registerAddress @reg) $ coerce $ r'
    pure r'

regmodify2 :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, MonadIO m) => 
            BusDevice chip -> (reg -> reg) -> m reg
regmodify2 busdev = \f -> do
    r <- regread2 busdev
    let r' = f r
    smbuswrite2 busdev (registerAddress @reg) $ coerce $ r'
    pure r'

