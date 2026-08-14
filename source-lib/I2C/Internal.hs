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

    BusDevice (..),
    openChip,
    closeChip,

    Register (..),

    rawread,
    rawwrite,
    rawmodify,

    regreadRaw,
    regwriteRaw,
    regmodifyRaw,
    regread0,
    regwrite0,
    regmodify0,
    regread1,
    regwrite1,
    regmodify1,
    regread2,
    regwrite2,
    regmodify2,

    regreadRegister1,
    regwriteRegister1,
    regmodifyRegister1,
    regreadRegister2,
    regwriteRegister2,
    regmodifyRegister2,

) where

import Relude 
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
-- raw read and write to chip without registers (SMBus free). 
-- size determined by 'Storable a'

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

----------------------------------------------------------------------------------
-- raw read and write to chip with registers (SMBus free). 
-- size determined by 'Storable a'

regreadRaw :: forall chip a m . (Chip chip, Storable a, MonadIO m) => BusDevice chip -> RegisterAddress -> m a
regreadRaw busdev addr = liftIO $
    readRegRaw @chip busdev addr 

regwriteRaw :: forall chip a m . (Chip chip, Storable a, MonadIO m)  => BusDevice chip -> RegisterAddress -> a -> m ()
regwriteRaw busdev addr = \a -> liftIO $
    writeRegRaw @chip busdev addr a

regmodifyRaw :: forall chip a m . (Chip chip, Storable a, MonadIO m)  => BusDevice chip -> RegisterAddress -> (a -> a) -> m a
regmodifyRaw busdev addr = \f -> liftIO $ do
    a <- regreadRaw @chip busdev addr
    let a' = f a
    regwriteRaw @chip busdev addr a'
    pure a'



--------------------------------------------------------------------------------
--  Word8 (smbus "byte")

---- | read single Word8 from chip (no register)
regread0 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> m Word8
regread0 busdev = liftIO $ 
    readData1 busdev 

-- | write single Word8 to chip (no register)
regwrite0 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> Word8 -> m ()
regwrite0 busdev = \w -> liftIO $ 
    writeData1 busdev w

-- | modify current single Word8 in chip (no register)
regmodify0 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> (Word8 -> Word8) -> m Word8
regmodify0 busdev = \f -> liftIO $ do
    w <- regread0 @chip busdev
    let w' = f w
    regwrite0 @chip busdev $ f w'
    pure w' 


--------------------------------------------------------------------------------
--  Word8 (register + smbus "byte")

-- | read 1 raw byte from register at chip
regread1 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> m Word8
regread1 busdev = \raddr -> liftIO $ 
    readRegData1 busdev raddr
         
-- | write raw byte to register at chip. returns value written
regwrite1 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> Word8 -> m ()
regwrite1 busdev = \addr w -> liftIO $ 
    writeRegData1 busdev addr w 

-- | modify current raw content of register
regmodify1 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> (Word8 -> Word8) -> m Word8
regmodify1 busdev = \addr f -> liftIO $ do
    w <- regread1 @chip busdev addr
    let w' = f w
    regwrite1 @chip busdev addr w'
    pure w'
    
--------------------------------------------------------------------------------
--  Word16 (register + smbus "word")

-- | read 2 raw bytes from register
regread2 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> m Word16
regread2 busdev = \addr -> liftIO $ 
    readRegData2 busdev addr 

-- | write 2 raw bytes to register. 
regwrite2 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> Word16 -> m ()
regwrite2 busdev = \addr w -> liftIO $ 
    writeRegData2 busdev addr w 

-- | modify current content of register
regmodify2 :: forall chip m . (Chip chip, MonadIO m) => BusDevice chip -> RegisterAddress -> (Word16 -> Word16) -> m Word16
regmodify2 busdev = \addr f -> liftIO $ do
    w <- regread2 @chip busdev addr
    let w' = f w
    regwrite2 busdev addr w'
    pure w'

--------------------------------------------------------------------------------
--  TODO: smbusxxxN (register + smbus "block")
--        1 byte length + n bytes data). according to spec, n is not allowed to 
--        be 0 for some reason, https://smbus.org/specs/smbus20.pdf#%5B%7B%22num%22%3A222%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22FitB%22%7D%5D


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
    regwrite  :: (Chip chip, Default reg, MonadIO m) => BusDevice chip -> (reg -> reg) -> m reg
    regmodify :: (Chip chip, MonadIO m) => BusDevice chip -> (reg -> reg) -> m reg


--------------------------------------------------------------------------------
--  helpers intented for Register instancing

regreadRegister1 :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, MonadIO m) => 
            BusDevice chip -> m reg
regreadRegister1 busdev = 
    fmap coerce $ regread1 @chip busdev (registerAddress @reg)

regwriteRegister1 :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, Default reg, MonadIO m) => 
            BusDevice chip -> (reg -> reg) -> m reg
regwriteRegister1 busdev = \f -> do
    let r' = f def
    regwrite1 @chip busdev (registerAddress @reg) (coerce $ r')
    pure r'

regmodifyRegister1 :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, MonadIO m) => 
            BusDevice chip -> (reg -> reg) -> m reg
regmodifyRegister1 busdev = \f -> do
    r <- regreadRegister1 busdev
    let r' = f r
    regwrite1 busdev (registerAddress @reg) $ coerce $ r'
    pure r'

     
regreadRegister2 :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, MonadIO m) => 
            BusDevice chip -> m reg
regreadRegister2 busdev =
    fmap coerce $ regread2 @chip busdev (registerAddress @reg)

regwriteRegister2 :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, Default reg, MonadIO m) => 
            BusDevice chip -> (reg -> reg) -> m reg
regwriteRegister2 busdev = \f -> do
    let r' = f def
    regwrite2 @chip busdev (registerAddress @reg) $ coerce $ r'
    pure r'

regmodifyRegister2 :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, MonadIO m) => 
            BusDevice chip -> (reg -> reg) -> m reg
regmodifyRegister2 busdev = \f -> do
    r <- regreadRegister2 busdev
    let r' = f r
    regwrite2 busdev (registerAddress @reg) $ coerce $ r'
    pure r'

