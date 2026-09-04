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
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module I2C.Internal.Linux
(
    BusDevice (..),
    openChip,
    closeChip,

    read,
    readSome,
    write,
    writeSome,

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

-- | connection to a hardware device on bus
data BusDevice chip = 
    BusDevice Text ChipAddress (Ptr I2C_Client) 


-- | instance Show
instance Chip chip => Show (BusDevice chip) where
    show (BusDevice id addr _ptr) = "(BusDevice " <> (toString $ chipName @chip) <> " " <> show addr <> "@" <> toString id <> ")"


-- | opens a connection to chip based on bus identifier and hardware address
openChip :: forall chip . (Chip chip) => Text -> ChipAddress -> IO (BusDevice chip)
openChip busid addr = do
    (try @IOException $ openFd (fromIdentifier busid) ReadWrite defaultFileFlags) >>= \case
        Left err   -> throwIO $ fromIOException err
        Right fd   -> do
            _ <- assertOK (tagErr busid addr) $ c_ioctl (fI fd) cpp_I2C_SLAVE_FORCE (fromChipAddress addr)
            pure $ BusDevice busid addr $ fdToPtrI2C_Client fd
    where
      fromIdentifier = toString
      tagErr busid addr = "openChip: could not find " <> chipName @chip <> " at " <> show addr <> " on bus " <> show busid
      fdToPtrI2C_Client = intPtrToPtr . fromIntegral 


-- | close connection to chip
closeChip :: forall chip . (Chip chip) => BusDevice chip -> IO ()
closeChip (BusDevice _id _addr ptr) = do
    (try @IOException $ closeFd $ ptrI2C_ClientToFd ptr) >>= \case
        Left err  -> throwIO $ fromIOException err
        Right _   -> pure ()
    where
      ptrI2C_ClientToFd = fromIntegral . ptrToIntPtr 


-- | set timeout for transfers
chipTimeoutMs :: forall chip . (Chip chip) => BusDevice chip -> Word -> IO ()
chipTimeoutMs busdev@(BusDevice _id _addr ptr) ms = do
    _ <- assertOK tagErr $ c_ioctl (ptrI2C_ClientToFd ptr) cpp_I2C_TIMEOUT $ fromIntegral $ div ms 10
    pure ()
    where
      tagErr = "chipTimeoutMs: could not set timeout to " <> show ms <> " ms on " <> show busdev
      ptrI2C_ClientToFd = fromIntegral . ptrToIntPtr 


--------------------------------------------------------------------------------
--  internal transaction API

-- |  read a specific amount of bytes determined by 'Storable r'. the reading
--    can be prefixed by a write of a specific amount of bytes determined by
--    'Storable w' if and only if 'sizeOf w' is non-zero. it is very
--    encouraged that the backend implement this as a "repeated START" 
--    transaction, since that is whole reason for the 'w' parameter.
--  
--      * call shall fail if 'w' can't be written fully.
--      * call shall fail if 'r' can't be read fully
--
read :: forall chip w r . (Chip chip, Storable w, Storable r)  => 
        BusDevice chip -> w -> IO r
read busdev@(BusDevice _id addr ptr) = \w -> do
    let sizeW = sizeOf w 
        sizeR = sizeOf (undefined :: r)
    when (maxTransferSize < (fI $ max sizeW sizeR)) $ throwIO $ errI2C eNOMEM $ tagErr busdev


    res <- try @IOException $ allocaBytes @Word8 (max sizeW sizeR) $ \mem -> do
        -- set write data. this data will be overwritten after reading
        poke (castPtr mem) w
        _ <- assertOK (tagErr busdev) $ c_i2c_read ptr (fromChipAddress addr) mem (fI sizeW) mem (fI sizeR)
        peek $ castPtr mem

    case res of
        Right a   -> pure a
        Left err  -> throwIO $ fromIOException err

    where
      tagErr busdev = "Internal.read " <> show busdev
    

-- |  read an arbitrary amount of bytes until NACK by slave. the reading
--    can be prefixed by a write of a specific amount of bytes determined by
--    'Storable w' if and only if 'sizeOf w' is non-zero. it is very
--    encouraged that the backend implement this as a "repeated START" 
--    transaction, since that is whole reason for the 'w' parameter.
--  
--      * call shall fail if 'w' can't be written fully.
--      * call can fail if the slave does not NACK after reading a larger number 
--        of bytes determined by the backend (typically by filling up a buffer).
--
readSome :: forall chip w . (Chip chip, Storable w) => BusDevice chip -> w -> IO ByteString
readSome busdev = \w ->
    throwIO $ errI2C eNOSYS "readSome not implemented on Linux"


-- |  write a specific amount of bytes determined by 'Storable w'.
--      * call shall fail if 'w' can't be written fully.
write :: forall chip w . (Chip chip, Storable w) => BusDevice chip -> w -> IO ()
write busdev@(BusDevice _id addr ptr) = \w -> do
    let sizeW = sizeOf w 
    when (maxTransferSize < (fI sizeW)) $ throwIO $ errI2C eNOMEM $ tagErr busdev

    res <- try @IOException $ allocaBytes @Word8 sizeW $ \mem -> do
        poke (castPtr mem) w
        _ <- assertOK (tagErr busdev) $ c_i2c_write ptr (fromChipAddress addr) mem (fI sizeW)
        pure ()
    case res of
        Right a   -> pure a
        Left err  -> throwIO $ fromIOException err
    where
      tagErr busdev = "Internal.write " <> show busdev
    
    

-- |  write an arbitrary amount of bytes until NACK by slave. returns the number
--    of bytes written.
--      * call shall fail if 'w' can't be written fully.
--      * call can fail if the slave does not NACK after reading a larger number 
--        of bytes determined by the backend (typically by filling up a buffer).
writeSome :: forall chip . (Chip chip) => BusDevice chip -> ByteString -> IO Word
writeSome busdev = \bs ->
    throwIO $ errI2C eNOSYS "writeSome not implemented on Linux"


-- | maximal number of bytes allowed in a transaction. note that for 'read' and 
--   'readSome' the size of the write part is included.
--   FIXME: find a suitable large value that does not overflow the stack when we
--          make the call to 'allocaBytes'
maxTransferSize :: Word
maxTransferSize = 128


fI :: (Integral a, Num b) => a -> b
fI = fromIntegral


-- | handle negative return value as exception (throw I2CErr)
assertOK :: Num b => Text -> IO CInt -> IO b
assertOK str ma = do
    res <- ma 
    if res < 0 then throwIO $ errI2C (Errno $ negate res) str
               else pure $ fromIntegral res
                  

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
cpp_I2C_SLAVE_FORCE :: CULong
cpp_I2C_SLAVE_FORCE = 0x0706

-- |  > /* set timeout in units of 10 ms */
--    > #define I2C_TIMEOUT 0x0702	
cpp_I2C_TIMEOUT :: CULong
cpp_I2C_TIMEOUT = 0x0702

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

