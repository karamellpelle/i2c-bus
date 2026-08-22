/*
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
*/

#include <errno.h>
#include <stddef.h>
#include <sys/ioctl.h>
#include <linux/types.h>
#include <linux/i2c.h>
#include <linux/i2c-dev.h>
#include <errno.h>
#include <unistd.h>
#include "foreign.h"

#if 0
--  from https://www.kernel.org/doc/html/latest/i2c/dev-interface.html :
--    > /*
--    >  * Using I2C Write, equivalent of
--    >  * i2c_smbus_write_word_data(file, reg, 0x6543)
--    >  */
--    > buf[0] = reg;
--    > buf[1] = 0x43;
--    > buf[2] = 0x65;
--    > if (write(file, buf, 3) != 3) {
--    >   /* ERROR HANDLING: I2C transaction failed */
--    > }
--    > 
--    > /* Using I2C Read, equivalent of i2c_smbus_read_byte(file) */
--    > if (read(file, buf, 1) != 1) {
--    >   /* ERROR HANDLING: I2C transaction failed */
--    > } else {
--    >   /* buf[0] contains the read byte */
-- > }
#endif

/*
readRaw :: forall chip a . (Chip chip, Storable a) => BusDevice chip -> IO a
readRaw busdev@(BusDevice _id ptr) = do
    let len = sizeOf @a undefined
    allocaArray (fromIntegral len) $ \arr -> do
        len' <- assertOK (tagErr busdev) $ fmap fromIntegral $ fdReadBuf (ptrI2C_ClientToFd ptr) arr (fromIntegral len)
        when (len' /= len) $ throwIO $ errI2C eIO $ "read " <> show len' <> " bytes, expected " <> show len
        (try @IOException $ peek (castPtr arr )) >>= \case
            Right a -> pure a
            Left  err -> throwIO $ fromIOException err
    where
      tagErr busdev = "readRaw " <> show busdev 
*/
int read_raw(int fd, uint8_t* buf, size_t len)
{
    if ( read( fd, buf, len ) != len ) return -EIO;

    return 0;
}

/*
writeRaw :: forall chip a . (Chip chip, Storable a) => BusDevice chip -> a -> IO ()
writeRaw busdev@(BusDevice _id ptr) = \a -> do
    let len = sizeOf a
    allocaArray (fromIntegral len) $ \arr -> do

        (try @IOException $ poke (castPtr arr) a) >>= \case
            Right _ -> pure ()
            Left err -> throwIO $ fromIOException err
        
        len' <- assertOK (tagErr busdev) $ fmap fromIntegral $ fdWriteBuf (ptrI2C_ClientToFd ptr) arr (fromIntegral len)
        when (len' /= len) $ throwIO $ errI2C eIO $ "wrote " <> show len' <> " bytes, expected " <> show len
    where
      tagErr busdev = "writeRaw " <> show busdev 
*/

int write_raw(int fd, const uint8_t* buf, size_t len)
{
    if ( write( fd, buf, len ) != len ) return -EIO;

    return 0;
}



// we assume that buf is non empty with buf[0] containing register address
int regread_raw(int fd, uint8_t* buf, size_t len)
{
    // FIXME: we should ideally have repeated start condition in order to keep line, 
    //        especially if there are multiple masters.
    //        looks like `i2c_transfer` is our function;
    //        https://www.kernel.org/doc/html//latest/driver-api/i2c.html#c.i2c_transfer

    // write register to tell device which data to read
    if ( write( fd, buf, 1 ) != 1 ) return -ECOMM;
    // read data from given register above
    if ( read( fd, buf, len ) != len ) return -ECOMM;

    return 0;
}

// we assume that buf is non empty with buf[0] has register written
int regwrite_raw(int fd, uint8_t* buf, size_t len)
{
    // write register + data
    if ( write( fd, buf, len + 1 ) != len + 1) return -EIO;

    return 0;
}

int i2c_read(int fd, uint8_t* wbuf, size_t wbuf_len, uint8_t* rbuf, size_t rbuf_len)
{
    struct i2c_client* client = (struct i2c_client*)( fd );
    /*struct i2c_adapter* adapter = client->adapter;*/
    struct i2c_msg messages[2]; // https://github.com/raspberrypi/linux/blob/65495647821026e14223095d1b0124aa3d502dec/include/uapi/linux/i2c.h#L74
    
    // write
    messages[0].addr = client->addr;
    messages[0].flags = 0;
    messages[0].len = wbuf_len;
    messages[0].buf = wbuf;
    // read
    messages[1].addr = client->addr;
    messages[1].flags = I2C_M_RD;
    messages[1].len = rbuf_len;
    messages[1].buf = rbuf;

    // transfer with repeated START
    // FIXME what is res? cf. comment https://github.com/raspberrypi/linux/blob/65495647821026e14223095d1b0124aa3d502dec/drivers/i2c/i2c-core-base.c#L2322
    int res = i2c_transfer( client->adapter, messages, 2 ); // https://github.com/raspberrypi/linux/blob/65495647821026e14223095d1b0124aa3d502dec/include/linux/i2c.h#L130
}

int i2c_read_some(int fd, uint8_t* wbuf, size_t wbuf_len, uint8_t* rbuf, size_t rbuf_len)
{
    return -EPERM;
}

int i2c_write(int fd, uint8_t* wbuf, size_t wbuf_len)
{
    struct i2c_client* client = (struct i2c_client*)( fd );
    /*struct i2c_adapter* adapter = client->adapter;*/
    struct i2c_msg messages[1]; // https://github.com/raspberrypi/linux/blob/65495647821026e14223095d1b0124aa3d502dec/include/uapi/linux/i2c.h#L74
    
    // write
    messages[0].addr = client->addr;
    messages[0].flags = 0;
    messages[0].len = wbuf_len;
    messages[0].buf = wbuf;

    // transfer data
    // FIXME what is res? cf. comment https://github.com/raspberrypi/linux/blob/65495647821026e14223095d1b0124aa3d502dec/drivers/i2c/i2c-core-base.c#L2322
    int res = i2c_transfer( client->adapter, messages, 1 ); // https://github.com/raspberrypi/linux/blob/65495647821026e14223095d1b0124aa3d502dec/include/linux/i2c.h#L130
}

int i2c_write_some(int fd, uint8_t* wbuf, size_t wbuf_len)
{
    return -EPERM;
}
