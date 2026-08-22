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
    -- export internal backend
#ifdef I2C_INTERNAL_LINUX
    module I2C.Internal.Linux,
#endif
#ifdef I2C_INTERNAL_EMPTY
    module I2C.Internal.Empty,
#endif

) where

import Relude 

#ifdef I2C_INTERNAL_LINUX
import I2C.Internal.Linux
#endif
#ifdef I2C_INTERNAL_EMPTY
import I2C.Internal.Empty
#endif

{-
-- Internal.X API
-- BusDevice chip (..)
-- openChip :: forall chip . (Chip chip) => Text -> IO (BusDevice chip)
-- closeChip :: forall chip . (Chip chip) => BusDevice chip -> IO ()
-- 
-- read :: Storable w, Storable r  => BusDevice chip -> w -> IO r
-- readSome :: Storable w => BusDevice chip -> w -> IO ByteString
-- write :: Storable w => BusDevice chip -> w -> IO ()
-- writeSome :: Storable w => BusDevice chip -> w -> IO ()


writeRegWord16 :: Word8 -> Word16 -> 
writeRegWord16 busdev reg w = 
    Internal.write busdev (reg + mkLE16 w)

newtype LE16 = LE16 Word16
#if PLATFORM_LITTLE_ENDIAN
    deriving (Storable)
#else 
instance Storable LE16 where
    poke =
    peek =
#endif

mkLE16 :: Word16 -> LE16
mkLE16 w = LE16 Word16
-}
