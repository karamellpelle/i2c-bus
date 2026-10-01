--------------------------------------------------------------------------------
-- | 
-- Module                  : I2C.Internal.Linux
-- SPDX-License-Identifier : MIT
-- Copyright               : karamellpelle@hotmail.com
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
-- Portability             : Linux
--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module I2C.Internal.Linux
(
    -- * Implementation of the backend API

    -- ** Chip connection
    BusDevice (..),
    openChip,
    closeChip,

    -- ** Chip communication
    write,
    read,
    writeSome,
    readSome,

    -- * Extra functionality on Linux 
    chipTimeoutMs,

) where

import Relude
import Relude.Extra.Newtype
import System.Posix.IO
import System.Posix.Types
import Numeric
import Text.Show qualified

import Foreign
import Foreign.C
import Control.Exception
import GHC.IO.Exception
import Data.Char (toUpper)

import I2C.Types
import I2C.Chip
import I2C.Exception



--------------------------------------------------------------------------------
--  

-- | A connection to a hardware device on bus 
data BusDevice chip = 
    BusDevice Text ChipAddress (Ptr I2C_Client) 


instance Chip chip => Show (BusDevice chip) where
    show (BusDevice id addr _ptr) = "(BusDevice " <> (toString $ chipName @chip) <> " " <> show addr <> "@" <> toString id <> ")"


-- | Open a connection to a chip of type 'chip' based on bus identifier and hardware address on bus.
--   The bus identifier on Linux is typically something like @\/dev\/i2c-N@.
--   May throw 'I2CErr'.
openChip :: forall chip . (Chip chip) => 
            Text ->                       -- ^ Bus identifier
            ChipAddress ->                -- ^ /7 bit/ hardware address
            IO (BusDevice chip)
openChip busid addr = do
    (try @IOException $ openFd (fromIdentifier busid) ReadWrite defaultFileFlags) >>= \case
        Left err   -> throwIO $ fromIOException err
        Right fd   -> do
            assertOK' (tagErr busid addr) $ c_ioctl (fI fd) c_I2C_SLAVE_FORCE (fromChipAddress addr)
            pure $ BusDevice busid addr $ fdToPtrI2C_Client fd
    where
      fromIdentifier = toString
      tagErr busid addr = "openChip: could not find " <> chipName @chip <> " at " <> show addr <> " on bus " <> show busid
      fdToPtrI2C_Client = intPtrToPtr . fromIntegral 


-- | Close connection to chip. 
--   Shall not throw 'I2CErr'.
closeChip :: forall chip . (Chip chip) => BusDevice chip -> IO ()
closeChip busdev@(BusDevice _id _addr ptr) = do
    (try @IOException $ closeFd $ ptrI2C_ClientToFd ptr) >>= \case
        Left err  -> throwIO $ fromIOException err
        Right _   -> pure ()
    where
      ptrI2C_ClientToFd = fromIntegral . ptrToIntPtr 


-- | Set timeout for transfers.
--   May throw 'I2CErr'.
chipTimeoutMs :: forall chip m . (Chip chip, MonadIO m) => 
                 BusDevice chip ->                -- ^ BusDevice
                 Word ->                          -- ^ Time in milliseconds
                 m ()
chipTimeoutMs busdev@(BusDevice _id _addr ptr) ms = liftIO $ do
    assertOK' tagErr $ c_ioctl (ptrI2C_ClientToFd ptr) c_I2C_TIMEOUT $ fromIntegral $ div ms 10
    pure ()
    where
      tagErr = "chipTimeoutMs: could not set timeout to " <> show ms <> " ms on " <> show busdev
      ptrI2C_ClientToFd = fromIntegral . ptrToIntPtr 


--------------------------------------------------------------------------------
--  internal transaction API

-- |  Write a specific amount of bytes.
--
--    May throw 'I2CErr', some specific cases are:
--
--      * Call shall fail if 'w' can't be written fully.
--
write :: forall chip w . (Chip chip) => 
         BusDevice chip ->              -- ^ BusDevice
         Int ->                         -- ^ Number of bytes to write 
         (Ptr w -> IO ())               -- ^ Write bytes 
         -> IO ()
write busdev@(BusDevice _id addr ptr) sizeW pokeW = do
    let withMem = if sizeW <= maxAllocaBytes then allocaBytes else mallocBytes'

    res <- try @IOException $ withMem sizeW $ \mem -> do
        pokeW $ castPtr mem
        assertOK' (tagErr busdev) $ c_i2c_write ptr (fromChipAddress addr) mem (fI sizeW)
        pure ()
    case res of
        Right a   -> pure a
        Left err  -> throwIO $ fromIOException err
    where
      tagErr busdev = "write " <> show busdev
      mallocBytes' size f = bracket (mallocBytes size) free f
    
-- |  Read a specific amount of bytes. The reading can be prefixed by a write 
--    of a given amount of bytes if that size is non-zero. 
--
--    It is very encouraged that the backend implement this as a "repeated START" 
--    transaction, since that's the whole reason for the write parameter. 
--
--    May throw 'I2CErr', some special cases are: 
--  
--      * Call shall fail if 'w' can't be written fully.
--      * Call shall fail if 'r' can't be read fully.
--
read :: forall chip w r . (Chip chip)  => 
        BusDevice chip ->                   -- ^ BusDevice
        Int ->                              -- ^ Number of bytes to write
        (Ptr w -> IO ()) ->                 -- ^ Write bytes
        Int ->                              -- ^ Number of bytes to read
        (Ptr r -> IO r) ->                  -- ^ Read bytes into type 'r' 
        IO r
read busdev@(BusDevice _id addr ptr) sizeW pokeW sizeR peekR = do
    let size = max sizeW sizeR
        withMem = if size <= maxAllocaBytes then allocaBytes else mallocBytes'

    res <- try @IOException $ withMem size $ \mem -> do
        -- set write data. this data will be overwritten when reading
        pokeW $ castPtr mem
        assertOK' (tagErr busdev) $ c_i2c_read ptr (fromChipAddress addr) mem (fI sizeW) mem (fI sizeR)
        peekR $ castPtr mem

    case res of
        Right a   -> pure a
        Left err  -> throwIO $ fromIOException err

    where
      tagErr busdev = "read " <> show busdev
      mallocBytes' size f = bracket (mallocBytes size) free f
    

--------------------------------------------------------------------------------
--  readSome / writeSome
--
--  TODO: define events at which we should throw 'I2CErr'


-- |  Write an arbitrary amount of bytes until completion or NACK by slave. Returns the number
--    of bytes written.
--
--    May throw 'I2CErr'. 
writeSome :: forall chip w . (Chip chip) => 
             BusDevice chip ->              -- ^ BusDevice
             Int ->                         -- ^ Number of bytes to write
             (Ptr w -> IO ()) ->            -- ^ Write bytes 
             IO Int
writeSome busdev sizeW pokeW =
    throwIO $ errI2C eNOSYS "writeSome not implemented on Linux"
{-# WARNING writeSome "Not implemented on Linux; throws 'I2CErr'" #-}

-- |  Read until NACK by slave or the specific amount of bytes have been read.
--    The reading can be prefixed by a write of a given amount of bytes 
--    if that size is non-zero. 
--
--    It is very encouraged that the backend implement this as a "repeated START" 
--    transaction, since that's the whole reason for the write parameter. 
--
--    May throw 'I2CErr', some special cases are: 
--  
--      * Call shall fail if 'w' can't be written fully.
--
readSome :: forall chip w r . (Chip chip) => 
            BusDevice chip ->                 -- ^ BusDevice
            Int ->                            -- ^ Number of bytes to write 
            (Ptr w -> IO ()) ->               -- ^ Write bytes 
            Int ->                            -- ^ Number of bytes to read 
            (Int -> Ptr r -> IO r)            -- ^ Read the given number of bytes into type 'r'. May throw 'I2CErr'.
            -> IO r
readSome busdev sizeW pokeW sizeR peekR' = 
    throwIO $ errI2C eNOSYS "readSome not implemented on Linux"
{-# WARNING readSome "Not implemented on Linux; throws 'I2CErr'" #-}


 
--------------------------------------------------------------------------------
--  

-- | the maximal number of bytes allowed in a transaction for stack allocation.
--   otherwise the memory is allocated on the heap. 
maxAllocaBytes :: Int
maxAllocaBytes = 128


fI :: (Integral a, Num b) => a -> b
fI = fromIntegral


-- | handle negative return value as exception (throw I2CErr)
assertOK :: Num b => Text -> IO CInt -> IO b
assertOK str ma = do
    res <- ma 
    if res < 0 then throwIO $ errI2C (Errno $ negate res) str
               else pure $ fromIntegral res
                  
assertOK' :: Text -> IO CInt -> IO ()
assertOK' str ma = do
    _ <- assertOK @CInt str ma
    pure ()

--------------------------------------------------------------------------------
--  FFI
--
--  resources:
--    * https://www.kernel.org/doc/html/latest/i2c/dev-interface.html
--    * https://www.kernel.org/doc/html/latest/driver-api/i2c.html
--
--  interesting settings (https://github.com/raspberrypi/linux/blob/ae4246632be85a9a7290a33b3d6c89c4ffa17d2b/include/uapi/linux/i2c-dev.h):
--    * ioctl(file, I2C_SLAVE, long addr): change slave address
--    * ioctl(file, I2C_FUNCS, unsigned long *funcs): get functionality
--    * ioctl(file, I2C_TIMEOUT, unsigned long *funcs): timeout in 10 ms
--    

-- | linux communication
data I2C_Client

-- |  > /* Use this slave address, even if it is already in use by a driver! */
--    > #define I2C_SLAVE_FORCE	0x0706	
c_I2C_SLAVE_FORCE :: CULong
c_I2C_SLAVE_FORCE = 0x0706

-- |  > /* set timeout in units of 10 ms */
--    > #define I2C_TIMEOUT 0x0702	
c_I2C_TIMEOUT :: CULong
c_I2C_TIMEOUT = 0x0702

-- | int ioctl(int d, int request, ...)
foreign import ccall safe "sys/ioctl.h ioctl" c_ioctl
    :: CInt -> CULong -> CInt -> IO CInt

-- | int i2c_read(int fd, uint8_t addr, uint8_t* wbuf, size_t wbuf_len, uint8_t* rbuf, size_t rbuf_len);
foreign import ccall safe "foreign.h i2c_read" c_i2c_read
    :: Ptr I2C_Client -> Word8 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO CInt

-- | int i2c_read_some(int fd, uint8_t addr, uint8_t* wbuf, size_t wbuf_len, uint8_t* rbuf, size_t rbuf_len);
foreign import ccall safe "foreign.h i2c_read_some" c_i2c_read_some
    :: Ptr I2C_Client -> Word8 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO CInt

-- | int i2c_write(int fd, uint8_t addr, uint8_t* wbuf, size_t wbuf_len);
foreign import ccall safe "foreign.h i2c_write" c_i2c_write
    :: Ptr I2C_Client -> Word8 -> Ptr Word8 -> CSize -> IO CInt

-- | int i2c_write_some(int fd, uint8_t addr, uint8_t* wbuf, size_t wbuf_len);
foreign import ccall safe "foreign.h i2c_write_some" c_i2c_write_some
    :: Ptr I2C_Client -> Word8 -> Ptr Word8 -> CSize -> IO CInt

