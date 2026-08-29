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
{-# LANGUAGE AllowAmbiguousTypes #-}
module I2C.Register
(
    Register (..),

) where

import Relude
import Data.Default
import Text.Show qualified
import Foreign

import I2C.Internal qualified as Internal
import I2C.Chip
import I2C.Types


--------------------------------------------------------------------------------
--  class Register
--

-- | typeclass for a chip's register. 
class Register reg where
    -- | register index inside Chip
    registerAddress :: RegisterAddress
    -- | human readable name
    registerName :: Text
    registerName = "unknown@" <> (show $ registerAddress @reg)

    -- | type of content in register
    type RegisterItem reg

    -- | bus functions
    regread   :: (Chip chip, MonadIO m) => Internal.BusDevice chip -> m reg
    regwrite  :: (Chip chip, MonadIO m) => Internal.BusDevice chip -> reg -> m ()
    regmodify :: (Chip chip, MonadIO m) => Internal.BusDevice chip -> (reg -> reg) -> m reg
    regmodify = \busdev f -> do
        r <- regread busdev
        let r' = f r
        regwrite busdev r'
        pure r'


