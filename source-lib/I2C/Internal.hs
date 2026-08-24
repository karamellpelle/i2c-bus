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
module I2C.Internal
(
    -- export selected backend
#ifdef INTERNAL_USE_LINUX
    module I2C.Internal.Linux,
#endif
#ifdef INTERNAL_USE_EMPTY
    module I2C.Internal.Empty,
#endif

) where

import Relude 

#ifdef INTERNAL_USE_LINUX
import I2C.Internal.Linux
#endif
#ifdef INTERNAL_USE_EMPTY
import I2C.Internal.Empty
#endif


-- Internal.X API
-- BusDevice chip (..)
-- openChip :: forall chip . (Chip chip) => Text -> IO (BusDevice chip)
-- closeChip :: forall chip . (Chip chip) => BusDevice chip -> IO ()

-- |  read a specific amount of bytes determined by 'Storable r'. the reading
--    can be prefixed by a write of a specific amount of bytes determined by
--    'Storable w' if and only if 'sizeOf w' is non-zero. it is very
--    encouraged that the backend implement this as a "repeated START" 
--    transaction, since that is whole reason for the 'w' parameter.
--  
--      * call shall fail if 'w' can't be written fully.
--      * call shall fail if 'r' can't be read fully
--
-- read :: Storable w, Storable r  => BusDevice chip -> w -> IO r

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
-- readSome :: Storable w => BusDevice chip -> w -> IO ByteString

-- write :: Storable w => BusDevice chip -> w -> IO ()
-- writeSome :: Storable w => BusDevice chip -> w -> IO ()
