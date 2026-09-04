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
module I2C.Register
(
    Register (..),

    regread,
    regwrite,
    regmodify,

    regread',
    regwrite',
    regmodify',
) where

import Relude
import Data.Default
import Text.Show qualified
import Foreign

import I2C.Internal qualified as Internal
import I2C.Chip
import I2C.Types



-- | index to a register of type 't' of a chip 
data Register chip t = Register Text RegisterAddress


regread :: (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> Register chip a -> m a
regread busdev (Register _name addr) = 
    liftIO $ Internal.read busdev addr

regwrite :: (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> Register chip a -> a -> m ()
regwrite busdev (Register _name addr) = \a ->
    liftIO $ Internal.write busdev $ StorableAB addr a

regmodify :: (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> Register chip a -> (a -> a) -> m a
regmodify = \busdev reg f -> do
    a <- regread busdev reg
    let a' = f a
    regwrite busdev reg a'
    pure a'


--------------------------------------------------------------------------------
--  raw addressing, no Register

regread' :: forall a chip m . (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> m a
regread' busdev addr = 
    liftIO $ Internal.read busdev addr

regwrite' :: forall a chip m . (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> a -> m ()
regwrite' busdev addr = \a ->
    liftIO $ Internal.write busdev $ StorableAB addr a

regmodify' :: forall a chip m . (Chip chip, Storable a, MonadIO m) => Internal.BusDevice chip -> RegisterAddress -> (a -> a) -> m a
regmodify' = \busdev addr f -> do
    a <- regread' busdev addr
    let a' = f a
    regwrite' busdev addr a'
    pure a'

