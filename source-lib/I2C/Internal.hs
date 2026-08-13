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

    Internal.BusDevice,
    Internal.openChip,
    Internal.closeChip,

    Register (..),

    regmodify1,
    regread1,
    regread2,
    --regreadN,
    regwrite1,
    regwrite2,
    --regwriteN,
    regmodify1,
    regmodify2,
    --regmodifyN,

    regread1',
    regread2',
    regwrite1',
    regwrite2',
    regmodify1',
    regmodify2',

) where

import Relude hiding (modify)
import Data.Default

import I2C.Chip

#ifdef I2C_INTERNAL_LINUX
import I2C.Internal.Linux qualified as Internal
#endif
#ifdef I2C_INTERNAL_EMPTY
import I2C.Internal.Empty qualified as Internal
#endif


--------------------------------------------------------------------------------
--  generic Register
--

-- | typeclass for a chip's register containing 1 Word8.
class Register reg where
    -- | register index inside Chip
    registerAddress :: RegisterAddress
    -- | human readable name
    registerName :: Text
    registerName = show $ registerAddress @reg

    -- | bus functions
    regread   :: (Chip chip, MonadIO m) => Internal.BusDevice chip -> m reg
    regwrite  :: (Chip chip, MonadIO m) => Internal.BusDevice chip -> (reg -> reg) -> m reg
    regmodify :: (Chip chip, MonadIO m) => Internal.BusDevice chip -> (reg -> reg) -> m reg




--------------------------------------------------------------------------------
--  working with bus devices having registers

              
--------------------------------------------------------------------------------
--  raw Word8 (smbus "byte")

---- | read single Word8 from chip (no register)
--read0 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> m Word8
--read0 busdev = liftIO $ 
--    Internal.readData0 busdev 
--
---- | write single Word8 to chip (no register)
--write0 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> Word8 -> m ()
--write0 busdev = \w -> liftIO $ 
--    Internal.writeData0 busdev w
--
---- | modify current single Word8 in chip (no register)
--modify0 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> (Word8 -> Word8) -> m Word8
--modify0 busdev = \f -> liftIO $ do
--    w <- read0 @chip busdev
--    let w' = f w
--    write0 @chip busdev $ f w'
--    pure w' 


--------------------------------------------------------------------------------
--  raw Word8 (register + smbus "byte")

-- | read 1 raw byte from register at chip
regread1 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> m Word8
regread1 busdev = \raddr -> liftIO $ 
    Internal.readData1 busdev raddr
         
-- | write raw byte to register at chip. returns value written
regwrite1 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> Word8 -> m ()
regwrite1 busdev = \addr w -> liftIO $ 
    Internal.writeData1 busdev addr w 

-- | modify current raw content of register
regmodify1 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> (Word8 -> Word8) -> m Word8
regmodify1 busdev = \addr f -> liftIO $ do
    w <- regread1 @chip busdev addr
    let w' = f w
    regwrite1 @chip busdev addr w'
    pure w'
    
--------------------------------------------------------------------------------
--  raw Word16 (register + smbus "word")

-- | read 1 raw byte from register
regread2 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> m Word16
regread2 busdev = \addr -> liftIO $ 
    Internal.readData2 busdev addr 

-- | write raw byte to register. returns value written
regwrite2 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> Word16 -> m ()
regwrite2 busdev = \addr w -> liftIO $ 
    Internal.writeData2 busdev addr w 

-- | modify current raw content of register
regmodify2 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> (Word16 -> Word16) -> m Word16
regmodify2 busdev = \addr f -> liftIO $ do
    w <- regread2 @chip busdev addr
    let w' = f w
    regwrite2 busdev addr w'
    pure w'

--------------------------------------------------------------------------------
--  raw [Word8] (register + smbus "block")

--regreadN :: forall chip reg m . (Register chip reg, MonadIO m) => Internal.BusDevice chip -> m ByteString
--regreadN busdev = liftIO $ 
--    Internal.readDataN busdev (registerAddress @chip @reg)
--
--regwriteN :: forall chip reg m . (Register chip reg, MonadIO m) => Internal.BusDevice chip -> ByteString -> m ByteString
--regwriteN busdev = \ws -> liftIO $ 
--    Internal.writeDataN busdev (registerAddress @chip @reg) ws >> pure ws
--
---- | modify current raw content of register
--regmodifyN :: forall chip reg m . (Register chip reg, MonadIO m) => Internal.BusDevice chip -> (ByteString -> ByteString) -> m ByteString
--regmodifyN busdev = \f -> liftIO $ do
--    ws <- regreadN @chip @reg busdev
--    regwriteN @chip @reg busdev $ f ws


----------------------------------------------------------------------------------
--  [Word] (SMBus free)
--
--rawreadN :: (Chip chip, MonadIO m)  => BusDev chip -> m ByteString
--rawreadN busdev = liftIO $
--    Internal.readRaw @chip busdev
--
----  read/write raw [Word]
--rawwriteN :: (Chip chip, MonadIO m)  => BusDev chip -> ByteString -> m ByteString
--rawwriteN busdev = \ws -> liftIO $
--    Internal.writeRaw @chip busdev ws
--
--rawread :: (Storable a, Chip chip, MonadIO m)  => BusDev chip -> m a
--rawread busdev = liftIO $
--    Internal.readRaw @chip busdev
--
--rawwrite :: (Storable a, Chip chip, MonadIO m)  => BusDev chip -> a -> m ()
--rawwrite busdev = \a -> liftIO $
--    Internal.writeRaw @chip busdev a


--------------------------------------------------------------------------------
--  working with Register instances

regread1' :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, MonadIO m) => 
            Internal.BusDevice chip -> m reg
regread1' busdev = 
    fmap coerce $ regread1 @chip busdev (registerAddress @reg)

regwrite1' :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, Default reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regwrite1' busdev = \f -> do
    let r' = f def
    regwrite1 @chip busdev (registerAddress @reg) (coerce $ r')
    pure r'

regmodify1' :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regmodify1' busdev = \f -> do
    r <- regread1' busdev
    let r' = f r
    regwrite1 busdev (registerAddress @reg) $ coerce $ r'
    pure r'

     
regread2' :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, MonadIO m) => 
            Internal.BusDevice chip -> m reg
regread2' busdev =
    fmap coerce $ regread2 @chip busdev (registerAddress @reg)

regwrite2' :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, Default reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regwrite2' busdev = \f -> do
    let r' = f def
    regwrite2 @chip busdev (registerAddress @reg) $ coerce $ r'
    pure r'

regmodify2' :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regmodify2' busdev = \f -> do
    r <- regread2' busdev
    let r' = f r
    regwrite2 busdev (registerAddress @reg) $ coerce $ r'
    pure r'

-- TODO: regxxxN

