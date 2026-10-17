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
import Control.Exception


--------------------------------------------------------------------------------
--  
-- to load this project in ghci: `stack ghci --test i2c-bus:test:ssd1306`
--
-- (you may need to do `stack build` first)
--
--------------------------------------------------------------------------------


main :: IO ()
main = do
    res <- try @SomeException $ do
        ssd <- openSSD1306 "/dev/i2c-1"
        ssd1306Init ssd

        display ssd =<< loadGIF "tests/ssd1306/haskell-128x32.gif"
        
    case res of 
        Left err  -> putTextLn $ show err
        Right _   -> pure ()

    where
      loops = 4 :: Word
      display :: SSD1306 -> [GIFFrame] -> IO ()
      display ssd frames = display' 0 ssd frames frames
      display' i ssd frames ((img, delay):fs) = do
          ssd1306Image ssd $ fromDynamicImage img
          threadDelay $ delay * 10000
          display' i ssd frames fs
      display' i ssd frames []
          | i == loops = pure ()
          | otherwise  = display' (succ i) ssd frames frames
    

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
    let mab = do
          imgs <- decodeGifImages bs
          delays <- getDelaysGifImages bs
          pure (imgs, delays)
    case mab of 
        Left err             -> (putTextLn $ toText err) >> pure mempty
        Right (imgs, delays) -> pure $ zip imgs delays
            

