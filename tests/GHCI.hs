{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module GHCI where

import Relude 
import Relude.Extra.Newtype
import I2C
import I2C.Internal
import I2C.Register
import I2C.Raw
import Data.Default
import Text.Show qualified
import Numeric
import Data.Bits
import Data.Char (toUpper)
import Text.Pretty.Simple
import Text.Printf
import Control.Concurrent

--
-- * load ghci with linux build:
--    $ stack ghci --package=pretty-simple  --flag=i2c:-build-empty --flag=i2c:build-linux
-- * load ghci with dummy build:
--    $ stack ghci --package=pretty-simple  --flag=i2c:build-empty --flag=i2c:-build-linux
--
-- then load this file:
--    ghci> :l tests/GHCI.hs
--  
-- if you get link errors, run `stack test` which will build the FFI parts,
-- and those symbols will then be available in GHCi


--------------------------------------------------------------------------------
--  dummy

{-
$(chip "Chip123" 0xDE)

$(register1 "REG_WORD8"  0x10 0x0A)
$(register2 "REG_WORD16" 0x20 0x0B0C)

testDummy :: IO ()
testDummy = do
    busdev <- openChip @Chip123 "/dev/i2c-1"

    a <- regread busdev
    b <- regread busdev

    pPrint $ "REG_WORD8:  " <> show (a :: REG_WORD8)
    pPrint $ "REG_WORD16: " <> show (b :: REG_WORD16)

    pPrint $ "Default REG_WORD8:  " <> show (def :: REG_WORD8)
    pPrint $ "Default REG_WORD16: " <> show (def :: REG_WORD16)
-}


--------------------------------------------------------------------------------
--  PCF8575

-- this is a GPIO expander chip with 2 bytes => 16 pins
-- (no registers, only write and read 2 bytes)
--  * https://www.ti.com/lit/ds/symlink/pcf8575.pdf
$(chip "PCF8575" 0x20)

testPCF8575 :: IO ()
testPCF8575 = do
    busdev <- openChip @PCF8575 "/dev/i2c-1"
    
    forM_ [0..0x00FF] $ \ix -> do
        rawwrite @PCF8575 @Word16 busdev ix
        threadDelay 400000

$(register1 "CTRL" 0x01 0xa0)
$(register2 "XTAL" 0x02 0x01)

--------------------------------------------------------------------------------
--  MPU6050

{-
-- this is a accelerometer and gyroscope chip with registers
--  * https://randomnerdtutorials.com/arduino-mpu-6050-accelerometer-gyroscope/
--  * https://github.com/adafruit/Adafruit_MPU6050/blob/master/Adafruit_MPU6050.cpp
--  * https://www.invensense.tdk.com/en-us/download-resource/ps-mpu-6000a-00-mpu-6000-and-mpu-6050-datasheet
--  * https://www.invensense.tdk.com/download-resource/rm-mpu-6000a-00-mpu-6000-and-mpu-6050-register-map-and-descriptions
$(chip "MPU6050" 0x68)

$(register1 "USER_CTRL" 106 0x00)

$(register1 "INT_PIN_CFG" 55 0x00)
$(register1 "PWR_MGMT_1" 107 0x40)

$(register2 "TEMP" 0x41 0x0000)

$(register2 "GYRO_XOUT" 0x43 0x0000)
$(register2 "GYRO_YOUT" 0x45 0x0000)
$(register2 "GYRO_ZOUT" 0x47 0x0000)

testMPU6050 :: IO ()
testMPU6050 = do
  busdev <- openChip @MPU6050 "/dev/i2c-1"

  regwrite1 busdev 0x6A $ 0b00000111
  regwrite1 busdev 0x6B $ 0b00000010 -- disable sleep and set PLL from Y axis gyroscope reference

  forever $ do
      -- acceleration:
      x <- regread2 busdev 0x3B
      y <- regread2 busdev 0x3D
      z <- regread2 busdev 0x3F
      -- gyroscope
      --x <- regread2 busdev 0x43
      --y <- regread2 busdev 0x45
      --z <- regread2 busdev 0x47
      -- temperature
      t <- regread2 busdev 0x41


      let x' = (fromIntegral x :: Int16)
      let y' = (fromIntegral y :: Int16)
      let z' = (fromIntegral z :: Int16)
      putTextLn $ toText @String $ printf "X: %+ 6d Y: %+ 6d Z: %+ 6d" x' y' z'
      --putTextLn $ toText @String $ printf "X: %04X Y: %04X Z: %04X, temp: %04X" x y z t

      let t' = (fromIntegral @Int16 @Double (fromIntegral @Word16 @Int16 t) / 340.0 + 36.53) 
      putTextLn $ toText @String $ printf "T: %+ 3.1f " t'

      putTextLn ""

      -- ^ output doesn't look right, maybe the register setups are wrong :)
      threadDelay 400000
-}
