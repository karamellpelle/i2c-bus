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
module SSD1306
(
    SSD1306,
    openSSD1306,
    ssd1306Init,
    ssd1306Image,

    ImageOLED,
    fromDynamicImage,
) where

import Relude
import Foreign
import Language.Haskell.TH
import I2C
import Codec.Picture
import Codec.Picture.Gif
import Data.ByteString qualified as BS
import Control.Concurrent (threadDelay)

-- to load this in ghci: `stack ghci --test i2c-bus:test:ssd1306`


--------------------------------------------------------------------------------
--  Command

data Command  = NOP

              | DisplayOff
              | DisplayOn
              | DisplayUseRAM Bool
              | DisplayInverse Bool
              | DisplayContrast Word8 -- 0 <= n < 256

              | RAMMode Word8
              | RAMPages Word8 Word8 
              | RAMColumns Word8 Word8

              | MapStartline Word8 -- 0 <= n <= 63
              | MapOffset Word8
              | MapMultiplex Word8 -- 15 <= n <= 63
              | MapSeg Bool
              | MapCom Bool

              | DriveComPins Bool Bool
              | DriveChargePump Word8
              | DrivePreCharge Word8
              | DriveClockDiv Word8
              | DriveVComh Word8

              | ScrollDisable


instance Storable Command where
    sizeOf _    = 3
    alignment _ = 1
    peek ptr    = pure NOP
    poke ptr    = \case
        NOP                  -> poke' nop nop nop
        DisplayOff           -> poke' 0xAE nop nop
        DisplayOn            -> poke' 0xAF nop nop
        DisplayUseRAM ram    -> poke' (if ram then 0xA4 else 0xA5) nop nop
        DisplayInverse inv   -> poke' (if inv then 0xA7 else 0xA6) nop nop
        DisplayContrast hex  -> poke' 0x81 hex nop

        RAMMode mode         -> poke' (0x20 .|. (0b00000011 .&. mode)) 0 0 
        -- ^ using 'nop' (0xE3) as placeholder here gave a weird bug that only 1 column was updated when writing to RAM :/ 
        RAMPages start end   -> poke' 0x22 start end
        RAMColumns start end -> poke' 0x21 start end

        MapMultiplex n       -> poke' 0xA8 n nop
        MapOffset off        -> poke' 0xD3 off nop
        MapStartline line    -> poke' (0x40 .|. (0b00111111 .&. line)) nop nop
        MapSeg enable        -> poke' (if enable then 0xA1 else 0xA0) nop nop
        MapCom enable        -> poke' (if enable then 0xC8 else 0xC0) nop nop

        DriveComPins alt lr  -> poke' 0xDA ( 0b00000010 .|. (if alt then 0b00010000 else 0) .|. (if lr then 0b00100000 else 0) ) nop
        DrivePreCharge n     -> poke' 0xD9 n nop
        DriveChargePump p    -> poke' 0x8D p nop
        DriveClockDiv div    -> poke' 0xD5 div nop
        DriveVComh level     -> poke' 0xDB (0b00111100 .|. shiftL level 2) nop

        ScrollDisable        -> poke' 0x2E nop nop
        where 
          nop = 0xE3
          poke' a0 a1 a2 = do
              pokeByteOff @Word8 ptr 0 a0
              pokeByteOff @Word8 ptr 1 a1
              pokeByteOff @Word8 ptr 2 a2
              

--------------------------------------------------------------------------------
-- | ImageOLED
--
--   a representation of the largest possible image in RAM:
--   128 columns and 64 rows divided by 8 pages,
--   each byte represents a column of 8 pixels
--
data ImageOLED = ImageOLED (Image PixelRGB8)


-- | the Blue pixel component of image defines the pixels of our screen
instance Storable ImageOLED where
    sizeOf _                   = fromIntegral $ 128 * (div 64 8)
    alignment _                = 1
    peek ptr                   = undefined
    poke ptr a@(ImageOLED img) = forM_ (range 0 $ sizeOf a) $ \ix -> do
        let w = 128
            h = 64
            i = mod ix w
            j = (div ix w) * 8
        pokeByteOff @Word8 ptr ix $ fromCell i j 0b00000001
        where
          range b e = if b == e then [] else b : range (b + 1) e
          fromCell i j 0 = 0x00
          fromCell i j x = (if pick i j then x else 0) .|. fromCell i (j + 1) (shiftL x 1)
          pick i j = 
              let w = imageWidth img 
                  h = imageHeight img 
                  PixelRGB8 _r _g b | 0 <= i && i < w && 0 <= j && j < h = pixelAt img i j
                                    | otherwise                          = PixelRGB8 0 0 0
              in  b /= 0


fromDynamicImage :: DynamicImage -> ImageOLED
fromDynamicImage = ImageOLED . convertRGB8 

--------------------------------------------------------------------------------
--  hardware

data SSD1306  = SSD1306 {
                ssd1306_BusDevice :: BusDevice SSD1306
              , ssd1306_Width :: Word
              , ssd1306_Height :: Word
              , ssd1306_VccExternal :: Bool
              }


instance Chip SSD1306 where
    chipName = "SSD1306"

$(register ''SSD1306 0x00 "COMMAND" ''Command)
$(register ''SSD1306 0x40 "IMAGE" ''ImageOLED)


openSSD1306 :: FilePath -> IO SSD1306
openSSD1306 busid = do
    
    -- change this to your hardware. see Adafruit_SSD1306.cpp for help.
    -- for example, my module is a 128x32 pixels screen:
    let address = 0x3C
        width = 128
        height = 32
        vccExternal = False

    busdev <- openChip "/dev/i2c-1" address
   
    pure $ SSD1306 {
            ssd1306_BusDevice = busdev
          , ssd1306_Width = width
          , ssd1306_Height = height
          , ssd1306_VccExternal = vccExternal
          }
    

-- | initialize a SSD1306 chip. 
--   based on the Adafruit_SSD1306 library, 
--   https://github.com/adafruit/Adafruit_SSD1306/blob/master/Adafruit_SSD1306.cpp
ssd1306Init :: SSD1306 -> IO ()
ssd1306Init ssd = do
    
    let busdev = ssd1306_BusDevice ssd
        width = ssd1306_Width ssd
        height = ssd1306_Height ssd
        vccExternal = ssd1306_VccExternal ssd

    regwrite busdev regCOMMAND $ DisplayOff

    regwrite busdev regCOMMAND $ DriveClockDiv 0x80

    regwrite busdev regCOMMAND $ MapMultiplex $ fromIntegral $ height - 1
    -- ^ does this overwrite previous image data? if so, try to set to max (63)

    regwrite busdev regCOMMAND $ MapOffset 0

    regwrite busdev regCOMMAND $ MapStartline 0

    regwrite busdev regCOMMAND $ DriveChargePump $ if vccExternal then 0x10 else 0x14

    -- memory mode 
    -- * 0b00: Horizontal: 
    --    increase column address pointer for each written byte. when pointer equals
    --    column end address, set pointer to 0 and increase page address pointer.
    --
    --    i.e. write a 8 pixel colum for each given byte, and jump down to next 
    --    row of 8 pixels columns and restart, when the columns defined by 'RAMColumn'
    --    are filled. 
    regwrite busdev regCOMMAND $ RAMMode 0b00 

    regwrite busdev regCOMMAND $ MapSeg True
    regwrite busdev regCOMMAND $ MapCom True

    let (alt, enable, contrast) | width == 128 && height == 32 = (False, False, 0x8F)
                                | width == 128 && height == 64 = (True,  False, if vccExternal then 0x9F else 0xCF)
                                | width == 96  && height == 16 = (False, False, if vccExternal then 0x10 else 0xAF)
                                | width == 64  && height == 32 = (True,  False, if vccExternal then 0x10 else 0xCF)
                                | otherwise                    = (False, False, 0x8F)
    regwrite busdev regCOMMAND $ DriveComPins alt enable
    regwrite busdev regCOMMAND $ DisplayContrast contrast

    regwrite busdev regCOMMAND $ DrivePreCharge $ if vccExternal then 0x22 else 0xF1

    regwrite busdev regCOMMAND $ DriveVComh 0x40

    regwrite busdev regCOMMAND $ DisplayUseRAM True

    regwrite busdev regCOMMAND $ DisplayInverse False

    regwrite busdev regCOMMAND $ ScrollDisable

    regwrite busdev regCOMMAND $ DisplayOn


-- | write image to RAM
ssd1306Image :: SSD1306 -> ImageOLED -> IO ()
ssd1306Image ssd img = do

    let busdev = ssd1306_BusDevice ssd
        width = ssd1306_Width ssd
        height = ssd1306_Height ssd
    
    -- end address 0xFF is OK since we use horizontal memory mode (we write columns before pages)
    regwrite busdev regCOMMAND $ RAMPages 0x00 0xFF

    regwrite busdev regCOMMAND $ RAMColumns 0x00 (0x00 + (fromIntegral $ width - 1))

    -- TODO: use scroll functionality to setup automatic scroll if image is too large for the screen hardware

    -- write image to RAM
    regwrite busdev regIMAGE img



