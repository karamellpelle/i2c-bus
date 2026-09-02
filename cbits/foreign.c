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

////////////////////////////////////////////////////////////////////////////////
// see `i2c_rdwr_ioctl_data` at https://www.kernel.org/doc/html/latest/i2c/dev-interface.html#full-interface-description
//
int i2c_read(int fd, uint8_t addr, uint8_t* wbuf, size_t wbuf_len, uint8_t* rbuf, size_t rbuf_len)
{
    // https://github.com/raspberrypi/linux/blob/65495647821026e14223095d1b0124aa3d502dec/include/uapi/linux/i2c.h#L74
    struct i2c_msg messages[2]; 
    
    // write
    messages[0].addr  = addr;
    messages[0].flags = 0;
    messages[0].len   = wbuf_len;
    messages[0].buf   = wbuf;
    // read
    messages[1].addr  = addr;
    messages[1].flags = I2C_M_RD;
    messages[1].len   = rbuf_len;
    messages[1].buf   = rbuf;

    struct i2c_rdwr_ioctl_data rdwr;
    
    // don't write if no data
    if ( wbuf_len == 0 )
    {
        rdwr.msgs = &messages[1];
        rdwr.nmsgs = 1;
    }
    else
    {
        rdwr.msgs = messages;
        rdwr.nmsgs = 2;
    }

    // transfer messages with repeated START (i.e. this is the kernel `i2c_transfer` function but in user API)
    int res = ioctl( fd, I2C_RDWR, &rdwr );
    
    // according to the source for the `i2ctransfer` program, ioctl returns the number of messages sent.
    // but also see comment https://github.com/raspberrypi/linux/blob/65495647821026e14223095d1b0124aa3d502dec/drivers/i2c/i2c-core-base.c#L2322
    if ( res < 0 )           return -errno;
    if ( res != rdwr.nmsgs ) return -EIO;
   
    // return number of bytes read
    return rbuf_len;
}

int i2c_read_some(int fd, uint8_t addr, uint8_t* wbuf, size_t wbuf_len, uint8_t* rbuf, size_t rbuf_len)
{
    // FIXME: maybe the i2c_msg flag "I2C_M_IGNORE_NAK: treat NACK from client as ACK" will let us
    //        treat NACK as success? 
    //        see https://www.kernel.org/doc/html/v5.14/i2c/i2c-protocol.html#modified-transactions
    // TODO: compare how the `i2ctransfer` program does this: https://www.kernel.org/pub/software/utils/i2c-tools/
    return -EPERM;
}

int i2c_write(int fd, uint8_t addr, uint8_t* wbuf, size_t wbuf_len)
{
    // https://github.com/raspberrypi/linux/blob/65495647821026e14223095d1b0124aa3d502dec/include/uapi/linux/i2c.h#L74
    struct i2c_msg messages[1]; 
    
    // write
    messages[0].addr  = addr;
    messages[0].flags = 0;
    messages[0].len   = wbuf_len;
    messages[0].buf   = wbuf;

    struct i2c_rdwr_ioctl_data rdwr;
    rdwr.msgs = messages;
    rdwr.nmsgs = 1;

    // transfer messages with repeated START (i.e. this is the `i2c_transfer` function in user API)
    int res = ioctl( fd, I2C_RDWR, &rdwr );
    
    // according to the source for the `i2ctransfer` program, ioctl returns the number of messages sent.
    // but also see comment https://github.com/raspberrypi/linux/blob/65495647821026e14223095d1b0124aa3d502dec/drivers/i2c/i2c-core-base.c#L2322
    if ( res < 0 )           return -errno;
    if ( res != rdwr.nmsgs ) return -EIO;
    
    return wbuf_len;
}

int i2c_write_some(int fd, uint8_t addr, uint8_t* wbuf, size_t wbuf_len)
{
    // FIXME: maybe the i2c_msg flag "I2C_M_IGNORE_NAK: treat NACK from client as ACK" will let us
    //        treat NACK as success? 
    //        see https://www.kernel.org/doc/html/v5.14/i2c/i2c-protocol.html#modified-transactions
    return -EPERM;
}


