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
--{-# LANGUAGE UndecidableInstances #-}
module I2C
(
    module I2C.Class,
    module I2C.TH,

    Internal.BusDevice,
    Internal.openChip,
    Internal.closeChip,

    --Register (..),

    regread1,
    regread2,
    --regreadN,
    regwrite1,
    regwrite2,
    --regwriteN,
    regmodify1,
    regmodify2,
    --regmodifyN,

    --read0,
    read1,
    read2,
    --readN,
    --write0,
    write1,
    write2,
    --writeN,
    --modify0,
    modify1,
    modify2,
    --modifyN,
) where

import Relude hiding (modify)
import Data.Default

import I2C.Class
import I2C.TH

#ifdef I2C_INTERNAL_LINUX
import I2C.Internal.Linux qualified as Internal
#endif
#ifdef I2C_INTERNAL_EMPTY
import I2C.Internal.Empty qualified as Internal
#endif


--------------------------------------------------------------------------------
--  register read/write

regread1 :: forall chip reg m . (Chip chip, Register1 reg, Coercible Word8 reg, MonadIO m) => 
            Internal.BusDevice chip -> m reg
regread1 busdev = 
    fmap coerce $ read1 @chip busdev (register1Address @reg)

regwrite1 :: forall chip reg m . (Chip chip, Register1 reg, Coercible Word8 reg, Default reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regwrite1 busdev = \f -> do
    let r' = f def
    write1 @chip busdev (register1Address @reg) (coerce $ r')
    pure r'

regmodify1 :: forall chip reg m . (Chip chip, Register1 reg, Coercible Word8 reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regmodify1 busdev = \f -> do
    r <- regread1 busdev
    let r' = f r
    write1 @chip busdev (register1Address @reg) (coerce $ r')
    pure r'
     
regread2 :: forall chip reg m . (Chip chip, Register2 reg, Coercible Word16 reg, MonadIO m) => 
            Internal.BusDevice chip -> m reg
regread2 busdev =
    fmap coerce $ read2 @chip busdev (register2Address @reg)

regwrite2 :: forall chip reg m . (Chip chip, Register2 reg, Coercible Word16 reg, Default reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regwrite2 busdev = \f -> do
    let r' = f def
    write2 @chip busdev (register2Address @reg) $ coerce $ r'
    pure r'

regmodify2 :: forall chip reg m . (Chip chip, Register2 reg, Coercible Word16 reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regmodify2 busdev = \f -> do
    r <- regread2 busdev
    let r' = f r
    write2 busdev (register2Address @reg) $ coerce $ r'
    pure r'

-- TODO: regxxxN


--------------------------------------------------------------------------------
--  generic Register
--
-- how can we, at compile time, decide which of regread1, regread2 and regreadN to use
-- based on given 'reg'?
--
-- > regread :: (Chip chip, Register reg) => Internal.BusDevice chip -> m reg
-- > regread busdev = regread1 | regread2 | regreadX
-- >     
-- > regwrite :: (Chip chip, Register reg) => Internal.BusDevice chip -> reg -> m ()
-- > regwrite busdev = regwrite1 | regwrite2 | regwriteX

--class Register reg where
--    regread ::  (Chip chip, MonadIO m) => Internal.BusDevice chip -> m reg
--    regwrite :: (Default reg, MonadIO m) => Internal.BusDevice chip -> (reg -> reg) -> m reg
--
--instance (Coercible Word8 reg, Register1 reg) => Register reg where
--    regread   = regread1 
--    regwrite  = regwrite1
--
--instance (Coercible Word16 reg, Register2 reg) => Register reg where
--    regread   = regread2 
--    regwrite  = regwrite2


--------------------------------------------------------------------------------
--  raw Word8 (smbus "byte")

-- | read single Word8 from chip (no register)
read0 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> m Word8
read0 busdev = liftIO $ 
    Internal.readData0 busdev 

-- | write single Word8 to chip (no register)
write0 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> Word8 -> m ()
write0 busdev = \w -> liftIO $ 
    Internal.writeData0 busdev w

-- | modify current single Word8 in chip (no register)
modify0 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> (Word8 -> Word8) -> m Word8
modify0 busdev = \f -> liftIO $ do
    w <- read0 @chip busdev
    let w' = f w
    write0 @chip busdev $ f w'
    pure w' 


--------------------------------------------------------------------------------
--  raw Word8 (register + smbus "byte")

-- | read 1 raw byte from register at chip
read1 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> m Word8
read1 busdev = \raddr -> liftIO $ 
    Internal.readData1 busdev raddr
         
-- | write raw byte to register at chip. returns value written
write1 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> Word8 -> m ()
write1 busdev = \addr w -> liftIO $ 
    Internal.writeData1 busdev addr w 

-- | modify current raw content of register
modify1 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> (Word8 -> Word8) -> m Word8
modify1 busdev = \addr f -> liftIO $ do
    w <- read1 @chip busdev addr
    let w' = f w
    write1 @chip busdev addr w'
    pure w'
    
--------------------------------------------------------------------------------
--  raw Word16 (register + smbus "word")

-- | read 1 raw byte from register
read2 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> m Word16
read2 busdev = \addr -> liftIO $ 
    Internal.readData2 busdev addr 

-- | write raw byte to register. returns value written
write2 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> Word16 -> m ()
write2 busdev = \addr w -> liftIO $ 
    Internal.writeData2 busdev addr w 

-- | modify current raw content of register
modify2 :: forall chip m . (Chip chip, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> (Word16 -> Word16) -> m Word16
modify2 busdev = \addr f -> liftIO $ do
    w <- read2 @chip busdev addr
    let w' = f w
    write2 busdev addr w'
    pure w'

--------------------------------------------------------------------------------
--  raw [Word8] (register + smbus "block")

--readN :: forall chip reg m . (Register chip reg, MonadIO m) => Internal.BusDevice chip -> m ByteString
--readN busdev = liftIO $ 
--    Internal.readDataN busdev (registerAddress @chip @reg)
--
--writeN :: forall chip reg m . (Register chip reg, MonadIO m) => Internal.BusDevice chip -> ByteString -> m ByteString
--writeN busdev = \ws -> liftIO $ 
--    Internal.writeDataN busdev (registerAddress @chip @reg) ws >> pure ws
--
---- | modify current raw content of register
--modifyN :: forall chip reg m . (Register chip reg, MonadIO m) => Internal.BusDevice chip -> (ByteString -> ByteString) -> m ByteString
--modifyN busdev = \f -> liftIO $ do
--    ws <- readN @chip @reg busdev
--    writeN @chip @reg busdev $ f ws


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
