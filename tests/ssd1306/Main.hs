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
import Foreign
import I2C
import Codec.Picture
import Codec.Picture.Gif
import Data.ByteString qualified as BS
import Control.Concurrent (threadDelay)

import Text.Show qualified

-- to load this in ghci: `stack ghci --test i2c-bus:test:ssd1306`

--------------------------------------------------------------------------------
--  hardware

--data SSD1306  = SSD1306 {
--                ssd1306_BusDevice :: BusDevice SSD1306
--              , ssd1306_Width :: Word
--              , ssd1306_Height :: Word
--              , ssd1306_VccExternal :: Bool
--              }
--          
--ssd1306_SizeRAM :: SSD1306 -> Word
--ssd1306_SizeRAM ssd = div (ssd1306_Width ssd * ssd1306_Height ssd) 8
-- $(instanceChip ''SSD1306)

address :: ChipAddress
address = 0x3C

width :: Word
width = 128

height :: Word
height = 32

sizeRAM :: Word
sizeRAM = width * (div (height + 7) 8)

vccExternal :: Bool
vccExternal = False -- False => Switch Cap VCC


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
    sizeOf _ = 3
    alignment _ = 1
    peek ptr = pure NOP
    poke ptr = \case
        NOP                  -> poke' nop nop nop
        DisplayOff           -> poke' 0xAE nop nop
        DisplayOn            -> poke' 0xAF nop nop
        DisplayUseRAM ram    -> poke' (if ram then 0xA4 else 0xA5) nop nop
        DisplayInverse inv   -> poke' (if inv then 0xA7 else 0xA6) nop nop
        DisplayContrast hex  -> poke' 0x81 hex nop

        --RAMMode mode           -> poke' (0x20 .|. (0b00000011 .&. mode)) nop nop
        RAMMode mode         -> poke' (0x20 .|. (0b00000011 .&. mode)) 0 0 -- using 'nop' (0xE3) gave a bug that was difficult to find!
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
--  ImageOLED

data ImageOLED = ImageOLED (Image PixelRGB8)

--instance Show ImageOLED where
--    show _  = "ImageOLED"

instance Storable ImageOLED where
    sizeOf _ = fromIntegral $ width * (div (height + 7) 8) -- "+ 7" for upward rounding, ceiling instead of truncate
    alignment _ = 1
    peek ptr = undefined
    poke ptr a@(ImageOLED img) = forM_ (range 0 $ sizeOf a) $ \ix -> do
        let i = mod ix $ fromIntegral width
            j = (div ix $ fromIntegral width) * 8
        pokeByteOff @Word8 ptr ix $ fromCell i j 0b00000001
        where
          fromCell i j 0 = 0x00
          fromCell i j x = (if pick i j then x else 0) .|. fromCell i (j + 1) (shiftL x 1)
          pick i j = 
              let w = imageWidth img 
                  h = imageHeight img 
                  PixelRGB8 _r _g b | 0 <= i && i < w && 0 <= j && j < h = pixelAt img i j
                                    | otherwise                          = PixelRGB8 0 0 0
              in  b /= 0


range :: (Eq a, Num a) => a -> a -> [a]
range b e = if b == e then [] else b : range (b + 1) e


--------------------------------------------------------------------------------
--  SSD1306

$(chip "SSD1306")

$(register ''SSD1306 0x00 "COMMAND" ''Command)
$(register ''SSD1306 0x40 "IMAGE" ''ImageOLED)


ssd1306Init :: BusDevice SSD1306 -> IO ()
ssd1306Init ssd1306 = do

    regwrite ssd1306 regCOMMAND $ DisplayOff

    regwrite ssd1306 regCOMMAND $ DriveClockDiv 0x80

    regwrite ssd1306 regCOMMAND $ MapMultiplex $ fromIntegral $ height - 1

    regwrite ssd1306 regCOMMAND $ MapOffset 0

    regwrite ssd1306 regCOMMAND $ MapStartline 0

    regwrite ssd1306 regCOMMAND $ DriveChargePump $ if vccExternal then 0x10 else 0x14

    -- memory mode 
    -- * 0b00: Horizontal: 
    --    increase column address pointer for each written byte. when pointer equals
    --    column end address, set pointer to 0 and increase page address pointer
    regwrite ssd1306 regCOMMAND $ RAMMode 0b00 

    regwrite ssd1306 regCOMMAND $ MapSeg True
    regwrite ssd1306 regCOMMAND $ MapCom True

    --if ((WIDTH == 128) && (HEIGHT == 32)) {
    --  comPins = 0x02;
    --  contrast = 0x8F;
    regwrite ssd1306 regCOMMAND $ DriveComPins False False
    regwrite ssd1306 regCOMMAND $ DisplayContrast 0x8F

    regwrite ssd1306 regCOMMAND $ DrivePreCharge $ if vccExternal then 0x22 else 0xF1

    regwrite ssd1306 regCOMMAND $ DriveVComh 0x40

    regwrite ssd1306 regCOMMAND $ DisplayUseRAM True

    regwrite ssd1306 regCOMMAND $ DisplayInverse False

    regwrite ssd1306 regCOMMAND $ ScrollDisable

    regwrite ssd1306 regCOMMAND $ DisplayOn


ssd1306Image :: BusDevice SSD1306 -> ImageOLED -> IO ()
ssd1306Image ssd1306 img = do
    
    -- end address 0xFF is OK since we use horizontal memory mode (we write columns before pages)
    regwrite ssd1306 regCOMMAND $ RAMPages 0x00 0xFF

    regwrite ssd1306 regCOMMAND $ RAMColumns 0x00 (0x00 + (fromIntegral $ width - 1))

    -- write image to RAM
    regwrite ssd1306 regIMAGE img


--------------------------------------------------------------------------------
--  

main :: IO ()
main = do
    ssd1306 <- openChip "/dev/i2c-1" address
    ssd1306Init ssd1306

    frames <- loadGIF "tests/ssd1306/image-128x32.gif"
    display ssd1306 frames frames
    
    where
      display ssd1306 frames [] =
          display ssd1306 frames frames
      display ssd1306 frames ((img, delay):fs) = do
          ssd1306Image ssd1306 $ ImageOLED $ convertRGB8 img
          threadDelay $ fromIntegral $ delay * 10000
          display ssd1306 frames fs
    

-- NOTE: it seems that JuicyPixels gives us the same width and height for
--       every frame, but that each frame may actually be smaller, 
--       because the bounds check for 'pixelAt' above failed. 
--       hence make sure to load GIFs with frames all having the same size.
--
--       to create gif from frames:
--       > magick -delay 20 -loop 0 *.png animated_loop.gif # 200 ms delay, infinite loop
--
loadGIF :: FilePath -> IO [(DynamicImage, GifDelay)]
loadGIF path = do
    bs <- BS.readFile path
    
    let Right imgs   = decodeGifImages bs
    let Right delays = getDelaysGifImages bs
    pure $ zip imgs delays
            

