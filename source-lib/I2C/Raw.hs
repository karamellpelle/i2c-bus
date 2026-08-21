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
module I2C.Raw
(
    rawread,
    rawwrite,
    rawmodify,

) where

import Relude 
import Foreign

import I2C.Internal qualified as Internal
import I2C.Chip

----------------------------------------------------------------------------------
-- raw read and write without registers
-- 

rawread :: forall chip a m . (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> m a
rawread busdev = liftIO $
    Internal.readRaw @chip busdev

rawwrite :: forall chip a m . (Chip chip, Storable a, MonadIO m)  => Internal.BusDevice chip -> a -> m ()
rawwrite busdev = \a -> liftIO $
    Internal.writeRaw @chip busdev a

rawmodify :: forall chip a m . (Chip chip, Storable a, MonadIO m)  => Internal.BusDevice chip -> (a -> a) -> m a
rawmodify busdev = \f -> liftIO $ do
    a <- rawread @chip busdev
    let a' = f a
    rawwrite @chip busdev a'
    pure a'


