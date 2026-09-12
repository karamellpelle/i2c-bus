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
import SSD1306

import Codec.Picture
import Codec.Picture.Gif
import Data.ByteString qualified as BS
import Control.Concurrent (threadDelay)


--------------------------------------------------------------------------------
--  
-- to load this project in ghci: `stack ghci --test i2c-bus:test:ssd1306`
--
-- (you may need to do `stack build` first)
--
--------------------------------------------------------------------------------


main :: IO ()
main = do
    ssd <- openSSD1306 "/dev/i2c-1"
    ssd1306Init ssd

    display ssd =<< loadGIF "tests/ssd1306/image-128x32.gif"
    
    where
      display ssd frames = display' ssd frames frames
      display' ssd frames ((img, delay):fs) = do
          ssd1306Image ssd $ fromDynamicImage img
          threadDelay $ fromIntegral $ delay * 10000
          display' ssd frames fs
      display' ssd frames [] =
          display' ssd frames frames
    

--------------------------------------------------------------------------------
-- NOTE: it seems that JuicyPixels reports the same width and height for
--       every frame, but that each frame may actually have a smaller size.
--       because I got bounds exception in the 'pixelAt' call above during testing.
--       hence make sure to load GIFs were all frames are having the same size.
--
--       to create a gif from frames:
--       > magick -delay 20 -loop 0 *.png animated_loop.gif # 200 ms delay, infinite loop
--

type GIFFrame = (DynamicImage, GifDelay)

loadGIF :: FilePath -> IO [GIFFrame]
loadGIF path = do
    bs <- BS.readFile path
    let Right imgs   = decodeGifImages bs
        Right delays = getDelaysGifImages bs

    pure $ zip imgs delays
            

