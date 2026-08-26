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

    readRaw,
    writeRaw,
    readRegRaw,
    writeRegRaw,


    readData1,
    readRegData1,
    readRegData2,
    --readDataN,
    writeData1,
    writeRegData1,
    writeRegData2,
    --writeDataN,

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
--  connect to hardware device on bus


-- | connection to a hardware device on bus
data BusDevice chip = 
    BusDevice Text (Ptr I2C_Client)


-- | instance Show
instance Chip chip => Show (BusDevice chip) where
    show (BusDevice id addr) = "(BusDevice " <> (toString $ chipName @chip) <> " " <> show addr <> "@" <> toString id <> ")"


-- | opens a connection to chip based on bus identifier
openChip :: forall chip . (Chip chip) => Text -> IO (BusDevice chip)
openChip ident = do
    (try @IOException $ openFd (fromIdentifier ident) ReadWrite defaultFileFlags) >>= \case
        Left err   -> throwIO $ fromIOException err
        Right fd   -> do
            let addr = chipAddress @chip
            _ <- assertOK (tagErr ident addr) $ c_ioctl (fI fd) cpp_I2C_SLAVE_FORCE (fromChipAddress addr)
            pure $ BusDevice @chip ident $ fdToPtrI2C_Client fd
    where
      fromIdentifier = toString
      tagErr ident addr = "openChip: could not find " <> chipName @chip <> " at " <> show addr <> " on bus " <> show ident

-- | close connection to chip
closeChip :: forall chip . (Chip chip) => BusDevice chip -> IO ()
closeChip (BusDevice _id ptr) = do
    (try @IOException $ closeFd $ ptrI2C_ClientToFd ptr) >>= \case
        Left err  -> throwIO $ fromIOException err
        Right _   -> pure ()


fdToPtrI2C_Client :: Fd -> Ptr I2C_Client
fdToPtrI2C_Client =
    intPtrToPtr . fromIntegral 

ptrI2C_ClientToFd :: Ptr I2C_Client -> Fd
ptrI2C_ClientToFd =
    fromIntegral . ptrToIntPtr 


--------------------------------------------------------------------------------
--  minimal API

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
read busdev@(BusDevice _id ptr) = \w -> do
    let sizeW = sizeOf w 
        sizeR = sizeOf (undefined :: r)
    when (maxTransferSize < (fI $ sizeW + sizeR)) $ throwIO $ errI2C eNOMEM $ tagErr busdev

    res <- try @IOException $ allocaBytes @Word8 (sizeW + sizeR) $ \mem -> do
        let ptrW = plusPtr mem 0
            ptrR = plusPtr mem sizeW
        -- set write value
        poke (castPtr ptrW) w
        _ <- assertOK (tagErr busdev) $ c_i2c_read ptr (fromChipAddress $ chipAddress @chip) ptrW (fI sizeW) ptrR (fI sizeR)

        peek $ castPtr ptrR
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
write busdev@(BusDevice _id ptr) = \w -> do
    let sizeW = sizeOf w 
    when (maxTransferSize < (fI sizeW)) $ throwIO $ errI2C eNOMEM $ tagErr busdev

    res <- try @IOException $ allocaBytes @Word8 sizeW $ \mem -> do
        poke (castPtr mem) w
        _ <- assertOK (tagErr busdev) $ c_i2c_write ptr (fromChipAddress $ chipAddress @chip) mem (fI sizeW)
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
writeSome :: forall chip w r . (Chip chip) => BusDevice chip -> ByteString -> IO Word
writeSome busdev = \bs ->
    throwIO $ errI2C eNOSYS "writeSome not implemented on Linux"


-- | maximal number of bytes allowed in a transaction. 
--   for 'read' and 'readSome' the size of the write part is included.
--   FIXME: find a suitable value that does not segfault the stack with
--   the call to 'allocaBytesAligned'
maxTransferSize :: Word
maxTransferSize = 100     -- 100: 4 bytes read + 96 bytes read

--------------------------------------------------------------------------------
--  raw I2C read/write, no registers (SMBus free)
--  according to doc of `allocaBytes`, the call to `allocaArray` should free memory if exception

-- |  read data in BusDevice
--  FIXME: do we need some alignment restrictions on 'a'? cf. docs for `Storable.alignment|peek`
--          however, 'allocaBytesAligned' perform this check
readRaw :: forall chip a . (Chip chip, Storable a) => BusDevice chip -> IO a
readRaw busdev@(BusDevice _id ptr) = do
    let len = sizeOf @a undefined
    res <- try @IOException $ allocaArray len $ \arr -> do
        _ <- assertOK (tagErr busdev) $ c_read_raw ptr arr (fromIntegral len)
        peek $ castPtr arr

    case res of
        Right a   -> pure a
        Left err  -> throwIO $ fromIOException err

    where
      tagErr busdev = "readRaw " <> show busdev

writeRaw :: forall chip a . (Chip chip, Storable a) => BusDevice chip -> a -> IO ()
writeRaw busdev@(BusDevice _id ptr) = \a -> do
    let len = sizeOf a
    res <- try @IOException $ allocaArray len $ \arr -> do
        --  
        poke (castPtr arr) a
        _ <- assertOK (tagErr busdev) $ c_write_raw ptr arr (fromIntegral len)
        pure ()

    case res of
        Right _   -> pure ()
        Left err  -> throwIO $ fromIOException err
    where
      tagErr busdev = "writeRaw " <> show busdev


readRegRaw :: forall chip a . (Chip chip, Storable a) => BusDevice chip -> RegisterAddress -> IO a
readRegRaw busdev@(BusDevice _id ptr) raddr = do
    let len = sizeOf @a undefined
    res <- try @IOException $ allocaArray (len + 1) $ \arr -> do
        
        poke arr $ fromRegisterAddress raddr

        _ <- assertOK (tagErr busdev) $ c_regread_raw ptr arr (fromIntegral len) -- note: not len + 1
        peek $ castPtr arr
       
    case res of
        Right a   -> pure a
        Left  err -> throwIO $ fromIOException err
    where
      tagErr busdev = "readRegRaw " <> show busdev 

writeRegRaw :: forall chip a . (Chip chip, Storable a) => BusDevice chip -> RegisterAddress -> a -> IO ()
writeRegRaw busdev@(BusDevice _id ptr) raddr = \a -> do
    let len = sizeOf a
    res <- try @IOException $ allocaArray (len + 1) $ \arr -> do
        
        poke arr $ fromRegisterAddress raddr  -- address
        poke (plusPtr arr 1) $ a              -- data

        _ <- assertOK (tagErr busdev) $ c_regwrite_raw ptr arr (fromIntegral len) -- note: not len + 1
        pure ()
       
    case res of
        Right a   -> pure a
        Left  err -> throwIO $ fromIOException err
    where
      tagErr busdev = "readRegRaw " <> show busdev 

--------------------------------------------------------------------------------
-- read/write without register (smbus-command)

readData1 :: forall chip . (Chip chip) => BusDevice chip -> IO Word8
readData1 busdev@(BusDevice _id ptr) = do
    assertOK (tagErr busdev) $ c_readByte ptr 
    where
      tagErr busdev = "readData1 " <> show busdev

writeData1 :: forall chip . (Chip chip) => BusDevice chip -> Word8 -> IO ()
writeData1 busdev@(BusDevice _id ptr) = \w -> do
    _ <- assertOK (tagErr busdev w) $ c_writeByte ptr (fromIntegral w)
    pure ()
    where
      tagErr busdev w = "writeData1 " <> show busdev <> " := " <> show w


--------------------------------------------------------------------------------
--  read/write 1 byte at register (smbus-byte)
 
readRegData1 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> IO Word8
readRegData1 busdev@(BusDevice _id ptr) regaddr = do
    assertOK (tagErr busdev regaddr) $ c_readByteData ptr (fromRegisterAddress regaddr)
    where
      tagErr busdev regaddr = "readRegData1 " <> show busdev <> " " <> show regaddr 

writeRegData1 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> Word8 -> IO ()
writeRegData1 busdev@(BusDevice _id ptr) regaddr = \w -> do
    _ <- assertOK (tagErr busdev regaddr w) $ c_writeByteData ptr (fromRegisterAddress regaddr) (fromIntegral w)
    pure ()
    where
      tagErr busdev regaddr w = "writeRegData1 " <> show busdev <> " " <> show regaddr <> " := " <> show w


--------------------------------------------------------------------------------
--  read/write 2 bytes at register (smbus-word)

readRegData2 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> IO Word16
readRegData2 busdev@(BusDevice _id ptr) regaddr = do
    assertOK (tagErr busdev regaddr) $ c_readWordData ptr (fromRegisterAddress regaddr)
    where
      tagErr busdev regaddr = "readRegData2 " <> show busdev <> " " <> show regaddr 

writeRegData2 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> Word16 -> IO ()
writeRegData2 busdev@(BusDevice _id ptr) regaddr = \w -> do
    _ <- assertOK (tagErr busdev regaddr w) $ c_writeWordData ptr (fromRegisterAddress regaddr) (fromIntegral w)
    pure ()
    where
      tagErr busdev regaddr w = "writeRegData2 " <> show busdev <> " " <> show regaddr <> " := " <> show w


fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

--------------------------------------------------------------------------------
--  FFI
--  resources:
--
--    * https://www.kernel.org/doc/html/latest/i2c/dev-interface.html
--    * https://www.kernel.org/doc/html/latest/driver-api/i2c.html
--    * https://github.com/torvalds/linux/blob/master/drivers/i2c/i2c-core-smbus.c
--
--  NOTE: smbus defines "byte" as Word8 and "word" as Word16 !



-- | linux communication
data I2C_Client

--data I2C_Adapter


-- | handle negative return value as exception (throw I2CErr)
assertOK :: Num b => Text -> IO CInt -> IO b
assertOK str ma = do
    res <- ma 
    if res < 0 then throwIO $ errI2C (Errno $ negate res) str
               else pure $ fromIntegral res
                  
-- | int ioctl(int d, int request, ...)
foreign import ccall safe "sys/ioctl.h ioctl" c_ioctl
    :: CInt -> CULong -> CInt -> IO CInt


--------------------------------------------------------------------------------
--  FFI

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



--------------------------------------------------------------------------------
--  constants

-- |  > /* Use this slave address, even if it is already in use by a driver! */
--    > #define I2C_SLAVE_FORCE	0x0706	
cpp_I2C_SLAVE_FORCE :: CULong
cpp_I2C_SLAVE_FORCE = 0x0706



foreign import ccall safe "foreign.h read_raw" c_read_raw
    :: Ptr I2C_Client -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall safe "foreign.h write_raw" c_write_raw
    :: Ptr I2C_Client -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall safe "foreign.h regread_raw" c_regread_raw
    :: Ptr I2C_Client -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall safe "foreign.h regwrite_raw" c_regwrite_raw
    :: Ptr I2C_Client -> Ptr Word8 -> CSize -> IO CInt


-- |  s32 i2c_smbus_read_byte(const struct i2c_client *client)¶
foreign import ccall safe "smbus.h i2c_smbus_read_byte" c_readByte
    :: Ptr I2C_Client -> IO CInt

-- |  s32 i2c_smbus_write_byte(const struct i2c_client *client, u8 value)¶
foreign import ccall safe "smbus.h i2c_smbus_write_byte" c_writeByte
    :: Ptr I2C_Client -> CUChar ->IO CInt

-- |  s32 i2c_smbus_read_byte_data(const struct i2c_client *client, u8 command)¶
foreign import ccall safe "smbus.h i2c_smbus_read_byte_data" c_readByteData
    :: Ptr I2C_Client -> CUChar -> IO CInt

-- |  s32 i2c_smbus_write_byte_data(const struct i2c_client *client, u8 command, u8 value)¶
foreign import ccall safe "smbus.h i2c_smbus_write_byte_data" c_writeByteData
    :: Ptr I2C_Client -> CUChar -> CUChar -> IO CInt

-- |  s32 i2c_smbus_read_word_data(const struct i2c_client *client, u8 command)¶
foreign import ccall safe "smbus.h i2c_smbus_read_word_data" c_readWordData
    :: Ptr I2C_Client -> CUChar -> IO CInt

-- |  s32 i2c_smbus_write_word_data(const struct i2c_client *client, u8 command, u16 value)¶
foreign import ccall safe "smbus.h i2c_smbus_write_word_data" c_writeWordData
    :: Ptr I2C_Client -> CUChar -> CUShort -> IO CInt

