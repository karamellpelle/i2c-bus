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
module I2C.Exception
(
    I2CErr (..),
    errI2C,
    fromIOException,

) where

import Relude
import Relude.Extra.Newtype

import Foreign.C
import Foreign.C.Error (Errno)
import GHC.IO.Exception
import Text.Show qualified


--------------------------------------------------------------------------------
--  exception

data I2CErr = I2CErr Errno Text


instance Exception I2CErr where
    displayException = show


instance Show I2CErr where
    show (I2CErr errno text) = 
        "I2CErr " <> show (un @CInt errno) <> " " <> case text of
            "" -> toString $ strErrno errno
            _  -> toString $ text


errI2C :: Errno -> Text -> I2CErr
errI2C = I2CErr


fromIOException :: IOException -> I2CErr
fromIOException err = 
    errI2C (wrap @Errno $ ioe_errno err ?: 1) $ toText (displayException err)

--------------------------------------------------------------------------------
--  

strErrno :: Errno -> Text
strErrno errno = case un @CInt errno of
    -- linux/include/uapi/asm-generic/errno-base.h
    1   -> "Operation not permitted"
    2   -> "No such file or directory"
    3   -> "No such process"
    4   -> "Interrupted system call"
    5   -> "I/O error"
    6   -> "No such device or address"
    7   -> "Argument list too long"
    8   -> "Exec format error"
    9   -> "Bad file number"
    10  -> "No child processes"
    11  -> "Try again"
    12  -> "Out of memory"
    13  -> "Permission denied"
    14  -> "Bad address"
    15  -> "Block device required"
    16  -> "Device or resource busy"
    17  -> "File exists"
    18  -> "Cross-device link"
    19  -> "No such device"
    20  -> "Not a directory"
    21  -> "Is a directory"
    22  -> "Invalid argument"
    23  -> "File table overflow"
    24  -> "Too many open files"
    25  -> "Not a typewriter"
    26  -> "Text file busy"
    27  -> "File too large"
    28  -> "No space left on device"
    29  -> "Illegal seek"
    30  -> "Read-only file system"
    31  -> "Too many links"
    32  -> "Broken pipe"
    33  -> "Math argument out of domain of func"
    34  -> "Math result not representable"
    --  linux/include/uapi/asm-generic/errno.h
    35  -> "Resource deadlock would occur"
    36  -> "File name too long"
    37  -> "No record locks available"
    38  -> "Invalid system call number"
    39  -> "Directory not empty"
    40  -> "Too many symbolic links encountered"
    42  -> "No message of desired type"
    43  -> "Identifier removed"
    44  -> "Channel number out of range"
    45  -> "Level 2 not synchronized"
    46  -> "Level 3 halted"
    47  -> "Level 3 reset"
    48  -> "Link number out of range"
    49  -> "Protocol driver not attached"
    50  -> "No CSI structure available"
    51  -> "Level 2 halted"
    52  -> "Invalid exchange"
    53  -> "Invalid request descriptor"
    54  -> "Exchange full"
    55  -> "No anode"
    56  -> "Invalid request code"
    57  -> "Invalid slot"
    59  -> "Bad font file format"
    60  -> "Device not a stream"
    61  -> "No data available"
    62  -> "Timer expired"
    63  -> "Out of streams resources"
    64  -> "Machine is not on the network"
    65  -> "Package not installed"
    66  -> "Object is remote"
    67  -> "Link has been severed"
    68  -> "Advertise error"
    69  -> "Srmount error"
    70  -> "Communication error on send"
    71  -> "Protocol error"
    72  -> "Multihop attempted"
    73  -> "RFS specific error"
    74  -> "Not a data message"
    75  -> "Value too large for defined data type"
    76  -> "Name not unique on network"
    77  -> "File descriptor in bad state"
    78  -> "Remote address changed"
    79  -> "Can not access a needed shared library"
    80  -> "Accessing a corrupted shared library"
    81  -> ".lib section in a.out corrupted"
    82  -> "Attempting to link in too many shared libraries"
    83  -> "Cannot exec a shared library directly"
    84  -> "Illegal byte sequence"
    85  -> "Interrupted system call should be restarted"
    86  -> "Streams pipe error"
    87  -> "Too many users"
    88  -> "Socket operation on non-socket"
    89  -> "Destination address required"
    90  -> "Message too long"
    91  -> "Protocol wrong type for socket"
    92  -> "Protocol not available"
    93  -> "Protocol not supported"
    94  -> "Socket type not supported"
    95  -> "Operation not supported on transport endpoint"
    96  -> "Protocol family not supported"
    97  -> "Address family not supported by protocol"
    98  -> "Address already in use"
    99  -> "Cannot assign requested address"
    100 -> "Network is down"
    101 -> "Network is unreachable"
    102 -> "Network dropped connection because of reset"
    103 -> "Software caused connection abort"
    104 -> "Connection reset by peer"
    105 -> "No buffer space available"
    106 -> "Transport endpoint is already connected"
    107 -> "Transport endpoint is not connected"
    108 -> "Cannot send after transport endpoint shutdown"
    109 -> "Too many references: cannot splice"
    110 -> "Connection timed out"
    111 -> "Connection refused"
    112 -> "Host is down"
    113 -> "No route to host"
    114 -> "Operation already in progress"
    115 -> "Operation now in progress"
    116 -> "Stale file handle"
    117 -> "Structure needs cleaning"
    118 -> "Not a XENIX named type file"
    119 -> "No XENIX semaphores available"
    120 -> "Is a named type file"
    121 -> "Remote I/O error"
    122 -> "Quota exceeded"
    123 -> "No medium found"
    124 -> "Wrong medium type"
    125 -> "Operation Canceled"
    126 -> "Required key not available"
    127 -> "Key has expired"
    128 -> "Key has been revoked"
    129 -> "Key was rejected by service"
    130 -> "Owner died"
    131 -> "State not recoverable"
    132 -> "Operation not possible due to RF-kill"
    133 -> "Memory page has hardware error"
    n   -> show n
