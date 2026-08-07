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
module I2C.Internal.Empty
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
import I2C.Class
import I2C.Exception
import Numeric
import Text.Show qualified
import Data.Char (toUpper)


showWord8 :: Word8 -> String
showWord8 = showWordN 2

showWord16 :: Word16 -> String
showWord16 = showWordN 4 

showWordN :: Integral a => Int -> a -> String
showWordN n w =
    let str = fmap toUpper $ showHex w ""
        n'  = if length str <= n then n - length str else 0
    in replicate n' '0' <> str


--------------------------------------------------------------------------------
--  connection to a hardware device on bus

data BusDevice chip = 
    BusDevice Text ChipAddress

instance Chip chip => Show (BusDevice chip) where
    show (BusDevice id addr) = "(BusDevice " <> (toString $ chipName @chip) <> " " <> show addr <> "@" <> toString id <> ")"


openChip :: forall chip . (Chip chip) => Text -> IO (BusDevice chip)
openChip identifier = do
    let addr = chipAddress @chip
        name = chipName @chip
        path = identifier 
    putTextLn $ "Empty.openChip   " <> name <> " " <> show addr <> "@" <> path
    pure $ BusDevice identifier addr

closeChip :: forall chip . (Chip chip) => BusDevice chip -> IO ()
closeChip busdev = do
    putTextLn $ "Empty.closeChip  " <> show busdev


--------------------------------------------------------------------------------
--  communication

-- | read Word8 aka "byte"
readData0 :: forall chip . (Chip chip) => BusDevice chip -> IO Word8
readData0 busdev = do
    let ret = 0xDE
    putTextLn $ "Empty.readData0    @" <> show busdev <> " == " <> (toText $ showWord8 ret)
    pure ret

-- | write Word8 aka "byte"
writeData0 :: forall chip . (Chip chip) => BusDevice chip -> Word8 -> IO ()
writeData0 busdev w = do
    putTextLn $ "Empty.writeData0   @" <> show busdev <> " := " <> (toText $ showWord8 w)

-- | read Word8 aka "byte" at address
readData1 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> IO Word8
readData1 busdev regaddr = do
    let ret = 0xAD
    putTextLn $ "Empty.readData1    @" <> show busdev <> " " <> show regaddr <> " == " <> (toText $ showWord8 ret)
    pure ret

-- | write Word8 aka "byte" at address
writeData1 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> Word8 -> IO ()
writeData1 busdev regaddr = \w -> do
    putTextLn $ "Empty.writeData1   @" <> show busdev <> " " <> show regaddr <> " := " <> (toText $ showWord8 w)

-- read Word16 aka "word" at address
readData2 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> IO Word16
readData2 busdev regaddr = do
    let ret = 0xBEEF
    putTextLn $ "Empty.readData2    @" <> show busdev <> " " <> show regaddr <> " == " <> (toText $ showWord16 ret)
    pure ret

-- write Word16 aka "word" at address
writeData2 :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> Word16 -> IO ()
writeData2 busdev regaddr = \w -> do
    putTextLn $ "Empty.writeData2   @" <> show busdev <> " " <> show regaddr <> " := " <> (toText $ showWord16 w)

---- read ByteString aka "block"
--readDataN :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> IO Word16
--readDataN busdev regaddr = do
--    putTextLn $ "Empty.readDataN    @" <> show busdev <> " " <> show regaddr <> " == " <> (toText $ showWord16 ret)
--    pure ret
--
---- write ByteString aka "block"
--writeDataN :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> Word16 -> IO ()
--writeDataN busdev regaddr w = do
--    putTextLn $ "Empty.writeDataN   @" <> show busdev <> " " <> show regaddr <> " := " <> (toText $ showWord16 w)
--    pure w


--------------------------------------------------------------------------------
--  even more raw operations

--readRaw :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> IO ByteString
--readRaw busdev regaddr = do
--    let ret = 0xBEEF
--    putTextLn $ "Empty.readRaw    @" <> show busdev <> " " <> show regaddr <> " == " <> (toText $ showWord16 ret)
--    pure ret
--
--writeRaw :: forall chip . (Chip chip) => BusDevice chip -> RegisterAddress -> ByteString -> IO ()
--writeRaw busdev regaddr = \ws -> do
--    putTextLn $ "Empty.writeRaw   @" <> show busdev <> " " <> show regaddr <> " := " <> (toText $ showWord16 w)
