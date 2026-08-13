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
{-# LANGUAGE CPP #-}
module I2C.Internal.Linux
(
    BusDevice (..),
    openChip,
    closeChip,

    readData0,
    readData1,
    readData2,
    --readDataN,
    writeData0,
    writeData1,
    writeData2,
    --writeDataN,

    --readRaw
    --writeRaw,
    

) where

import Relude
import Relude.Extra.Newtype
import I2C.Chip
import I2C.Exception
import System.Posix.IO
import System.Posix.Types
import Numeric
import Text.Show qualified

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.Error
import Control.Exception
import GHC.IO.Exception
import Data.Char (toUpper)

showWord8 :: Word8 -> String
showWord8 w | 0x10 <= w   = fmap toUpper $ showHex w ""
            | otherwise   = fmap toUpper $ "0" <> showHex w ""
--------------------------------------------------------------------------------
--  connect to hardware device on bus

-- | /* Use this slave address, even if it is already in use by a driver! */
#define I2C_SLAVE_FORCE	0x0706	


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
            _ <- assertOK (tagErr ident addr) $ c_ioctl (fromIntegral fd) I2C_SLAVE_FORCE (fromChipAddress addr) 
            pure $ BusDevice @chip ident $ fdToPtrI2C_Client fd
    where
      fromIdentifier = toString
      tagErr ident addr = "openChip " <> show addr <> "@" <> show ident

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
-- read/write without register (smbus-command)

readData0 :: forall chip . (Chip chip) => BusDevice chip -> IO Word8
readData0 busdev@(BusDevice _id ptr) = do
    assertOK (tagErr busdev) $ c_readByte ptr 
    where
      tagErr busdev = "readData0 " <> show busdev

writeData0 :: forall chip . (Chip chip) => BusDevice chip -> Word8 -> IO ()
writeData0 busdev@(BusDevice _id ptr) = \w -> do
    _ <- assertOK (tagErr busdev w) $ c_writeByte ptr (fromIntegral w)
    pure ()
    where
      tagErr busdev w = "writeData0 " <> show busdev <> " := " <> show w


--------------------------------------------------------------------------------
--  read/write 1 byte at register (smbus-byte)
 
readData1 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> IO Word8
readData1 busdev@(BusDevice _id ptr) regaddr = do
    assertOK (tagErr busdev regaddr) $ c_readByteData ptr (fromRegisterAddress regaddr)
    where
      tagErr busdev regaddr = "readData1 " <> show busdev <> " " <> show regaddr 

writeData1 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> Word8 -> IO ()
writeData1 busdev@(BusDevice _id ptr) regaddr = \w -> do
    _ <- assertOK (tagErr busdev regaddr w) $ c_writeByteData ptr (fromRegisterAddress regaddr) (fromIntegral w)
    pure ()
    where
      tagErr busdev regaddr w = "writeData1 " <> show busdev <> " " <> show regaddr <> " := " <> show w


--------------------------------------------------------------------------------
--  read/write 2 byte at register (smbus-word)

readData2 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> IO Word16
readData2 busdev@(BusDevice _id ptr) regaddr = do
    assertOK (tagErr busdev regaddr) $ c_readWordData ptr (fromRegisterAddress regaddr)
    where
      tagErr busdev regaddr = "readData2 " <> show busdev <> " " <> show regaddr 

writeData2 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> Word16 -> IO ()
writeData2 busdev@(BusDevice _id ptr) regaddr = \w -> do
    _ <- assertOK (tagErr busdev regaddr w) $ c_writeWordData ptr (fromRegisterAddress regaddr) (fromIntegral w)
    pure ()
    where
      tagErr busdev regaddr w = "writeData2 " <> show busdev <> " " <> show regaddr <> " := " <> show w



--------------------------------------------------------------------------------
--  raw I2C read/write, no registers (SMBus free)

--  TODO: use C's `read`/`write` on file descriptor.
--  from https://www.kernel.org/doc/html/latest/i2c/dev-interface.html :
-- > /*
-- >  * Using I2C Write, equivalent of
-- >  * i2c_smbus_write_word_data(file, reg, 0x6543)
-- >  */
-- > buf[0] = reg;
-- > buf[1] = 0x43;
-- > buf[2] = 0x65;
-- > if (write(file, buf, 3) != 3) {
-- >   /* ERROR HANDLING: I2C transaction failed */
-- > }
-- > 
-- > /* Using I2C Read, equivalent of i2c_smbus_read_byte(file) */
-- > if (read(file, buf, 1) != 1) {
-- >   /* ERROR HANDLING: I2C transaction failed */
-- > } else {
-- >   /* buf[0] contains the read byte */
-- > }

--readRaw :: forall chip . (Chip chip) => BusDevice chip -> IO ByteString         -- or something
--writeRaw :: forall chip . (Chip chip) => BusDevice chip -> ByteString -> IO ()  -- or something

--------------------------------------------------------------------------------
--  FFI
--  resources:
--
--    * https://www.kernel.org/doc/html/latest/i2c/dev-interface.html
--    * https://www.kernel.org/doc/html/latest/driver-api/i2c.html
--    * https://github.com/torvalds/linux/blob/master/drivers/i2c/i2c-core-smbus.c
--
--  NOTE: smbus defines "byte" as Word8 and "word" as Word16 !


--data I2C_Adapter

-- | linux communication
data I2C_Client

-- | handle negative return value as exception (throw I2CErr)
assertOK :: Num b => Text -> IO CInt -> IO b
assertOK str ma = do
    res <- ma 
    if res < 0 then throwIO $ errI2C (Errno $ negate res) str
               else pure $ fromIntegral res
                  

foreign import ccall safe "sys/ioctl.h ioctl" c_ioctl
    :: CInt -> CULong -> CInt -> IO CInt


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

-- |  s32 i2c_smbus_read_block_data(const struct i2c_client *client, u8 command, u8 *values)¶
--foreign import ccall safe "smbus.h i2c_smbus_read_block_data" c_readBlockData
--    :: Ptr I2C_Client -> CUChar -> Ptr CUChar -> IO CInt
-- | s32 i2c_smbus_write_block_data(const struct i2c_client *client, u8 command, u8 length, const u8 *values)¶
--foreign import ccall safe "smbus.h i2c_smbus_write_i2c_block_data" c_writeI2CBlockData
--    :: Ptr I2C_Client -> CUChar -> CUChar -> Ptr CUChar -> IO CInt

