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
{-# LANGUAGE TemplateHaskell #-}
module Main
(
    main,
) where

import Relude
import I2C

import Foreign


--------------------------------------------------------------------------------
--  hardware

address :: ChipAddress
address = 0x3D

width :: Word
width = 128

height :: Word
height = 32

vccExternal :: Bool
vccExternal = False -- False => Switch Cap VCC

--------------------------------------------------------------------------------
--  Command

data Command  = NOP
              | DisplayOff
              | DisplayOn
              | DisplayClockDiv Word8
              | SetMultiplex Word8 -- 15 <= n <= 63
              | SetDisplayOffset Word8
              | SetStartLine Word8 -- 0 <= n <= 63
              | ChargePump Word8
              | LowerColumnStart Word8    -- FIXME: why, this is only for page MemoryMode
              | HigherColumnStart Word8   -- FIXME: why, this is only for page MemoryMode
              | MemoryMode Word8
              | SegRemapOff
              | SegRemapOn
              | ComRemapOff
              | ComRemapOn
              | ComPins Bool Bool
              | SetContrast Word8 -- 0 <= n < 256
              | PreCharge Word8
              | PageAddr Word8 Word8 
              | ColumnAddr Word8 Word8
              | VComh Word8
              | DisplayEntireOn
              | DisplayGDDRAM
              | DisplayInversePixels Bool
              | ScrollDisable


instance Storable Command where
    sizeOf _ = 3
    alignment _ = 1
    peek ptr =
        pure NOP
    poke ptr = \case
        NOP                       -> poke' nop nop nop
        DisplayOff                -> poke' 0xAE nop nop
        DisplayOn                 -> poke' 0xAF nop nop
        DisplayClockDiv div       -> poke' 0xD5 div nop
        SetMultiplex n            -> poke' 0xA8 n nop
        SetDisplayOffset off      -> poke' 0xD3 off nop
        SetStartLine line         -> poke' (0x40 .|. (0b00111111 .&. line)) nop nop
        ChargePump p              -> poke' 0x8D p nop
        LowerColumnStart x        -> poke' (0x00 .|. (0b00001111 .&. x)) nop nop -- FIXME: how does this work when also command uses 0x00?
        HigherColumnStart x       -> poke' (0x10 .|. (0b00001111 .&. x)) nop nop
        MemoryMode mode           -> poke' (0x20 .|. (0b00001111 .&. mode)) nop nop
        SegRemapOff               -> poke' 0xA0 nop nop
        SegRemapOn                -> poke' 0xA1 nop nop
        ComRemapOff               -> poke' 0xC0 nop nop
        ComRemapOn                -> poke' 0xC8 nop nop
        ComPins alt lr            -> poke' 0xDA ( 0b00000010 .|. (if alt then 0b000100000 else 0) .|. (if lr then 0b00100000 else 0) ) nop
        SetContrast hex           -> poke' 0x81 hex nop
        PreCharge n               -> poke' 0xD9 n nop
        PageAddr start end        -> poke' 0x22 start end
        ColumnAddr start end      -> poke' 0x21 start end
        VComh level               -> poke' 0xDB (0b00111100 .|. shiftL level 2) nop
        DisplayEntireOn           -> poke' 0xA5 nop nop
        DisplayGDDRAM             -> poke' 0xA4 nop nop
        DisplayInversePixels inv  -> poke' (if inv then 0xA7 else 0xA6) nop nop
        ScrollDisable             -> poke' 0x2E nop nop
        where 
          nop = 0xE3
          poke' a0 a1 a2 = do
              pokeByteOff @Word8 ptr 0 a0
              pokeByteOff @Word8 ptr 1 a1
              pokeByteOff @Word8 ptr 2 a2
              
--------------------------------------------------------------------------------
--  ImageOLED

data ImageOLED = ImageOLED

instance Storable ImageOLED where
    sizeOf _ = fromIntegral $ width * (div (height + 7) 8) -- "+ 7" rounds upwards, ceiling 
    alignment _ = 1
    peek ptr = pure ImageOLED
    poke ptr img = do

        --uint16_t count = WIDTH * ((HEIGHT + 7) / 8);
        --uint8_t *ptr = buffer;
        --if (wire) { // I2C
        --  wire->beginTransmission(i2caddr);
        --  WIRE_WRITE((uint8_t)0x40);
        --  uint16_t bytesOut = 1;
        --  while (count--) {
        --    if (bytesOut >= WIRE_MAX) {
        --      wire->endTransmission();
        --      wire->beginTransmission(i2caddr);
        --      WIRE_WRITE((uint8_t)0x40);
        --      bytesOut = 1;
        --    }
        --    WIRE_WRITE(*ptr++);
        --    bytesOut++;
        --  }
        --  wire->endTransmission();
        --
        pure ()



--------------------------------------------------------------------------------
--  SSD1306

$(chip "SSD1306")

$(register ''SSD1306 0x00 "COMMAND" ''Command)
$(register ''SSD1306 0x40 "IMAGE" ''ImageOLED)
-- $(register ''SSD1306 0x40 "IMAGE_RAW" ''[Word8])



ssd1306Init :: BusDevice SSD1306 -> IO ()
ssd1306Init ssd1306 = do
      -- create buffer
      -- clear bitmap



    regwrite ssd1306 regCOMMAND $ DisplayOff

    regwrite ssd1306 regCOMMAND $ DisplayClockDiv 0x80

    regwrite ssd1306 regCOMMAND $ SetMultiplex $ fromIntegral $ height - 1

    regwrite ssd1306 regCOMMAND $ SetDisplayOffset 0

    regwrite ssd1306 regCOMMAND $ SetStartLine 0

    regwrite ssd1306 regCOMMAND $ ChargePump $ if vccExternal then 0x10 else 0x144

    -- memory mode 
    -- * 0b00: Horizontal: 
    --    increase column address pointer for each written byte. when pointer equals
    --    column end address, set pointer to 0 and increase page address pointer
    regwrite ssd1306 regCOMMAND $ MemoryMode 0b00 -- 0x00 : increase columns, if 

    regwrite ssd1306 regCOMMAND $ SegRemapOn

    regwrite ssd1306 regCOMMAND $ ComRemapOn

    --if ((WIDTH == 128) && (HEIGHT == 32)) {
    --  comPins = 0x02;
    --  contrast = 0x8F;
    regwrite ssd1306 regCOMMAND $ ComPins False True
    regwrite ssd1306 regCOMMAND $ SetContrast 0x8F

    regwrite ssd1306 regCOMMAND $  PreCharge $ if vccExternal then 0x22 else 0xF1

    regwrite ssd1306 regCOMMAND $ VComh 0x40

    regwrite ssd1306 regCOMMAND $ DisplayGDDRAM

    regwrite ssd1306 regCOMMAND $ DisplayInversePixels False

    regwrite ssd1306 regCOMMAND $ ScrollDisable

    regwrite ssd1306 regCOMMAND $ DisplayOn


ssd1306Image :: BusDevice SSD1306 -> ImageOLED -> IO ()
ssd1306Image ssd1306 img = do
    
    regwrite ssd1306 regCOMMAND $ ColumnAddr 0x00 (0x00 + (fromIntegral $ width - 1))

    -- end address 0xFF is OK since we use horizontal memory mode (we write columns before pages)
    regwrite ssd1306 regCOMMAND $ PageAddr 0x00 0xFF

    -- write image to GDDRAM
    regwrite ssd1306 regIMAGE img


loadImageOLED :: FilePath -> IO ImageOLED
loadImageOLED = undefined


ssd1306Clear :: BusDevice SSD1306 -> IO ()
ssd1306Clear = undefined


--------------------------------------------------------------------------------
--  

main :: IO ()
main = do
    ssd1306 <- openChip "/dev/i2c-1" address
    img <- loadImageOLED "tests/ssd1306/image-128x32.png"

    ssd1306Init  ssd1306
    ssd1306Clear ssd1306
    ssd1306Image ssd1306 img
    

