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
import I2C.Types
import Data.Default
import Text.Show qualified
import Numeric
import Data.Bits
import Data.Char (toUpper)
import Text.Pretty.Simple
import Text.Printf
import Control.Concurrent
import Foreign
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Data.Storable.Endian

--------------------------------------------------------------------------------
--
-- * load ghci (with extra package pretty-simple that provides handy `pPrint` function):
--    $ stack ghci --package=pretty-simple --package=storable-endian
--
-- then load this file:
--    ghci> :l tests/GHCI.hs
--  
-- if you get link errors, run `stack test` which will build the FFI parts,
-- and those symbols will then be available in GHCi
--  
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- test data

$(register8 "MY8" 0x22 0x83)
$(field    ''MY8   "A_FIELD"  "0000***0")
$(field    ''MY8   "A_BIT"    "00*00000")

$(register16LE "MY16" 0x44 0x1122)
$(field       ''MY16  "B_FIELD"  "00000000000****0")
$(field       ''MY16  "B_BIT"    "00*0000000000000")


a :: MY8
a = MY8 0b00000110

b :: MY16
b = MY16 0b0000111100011000


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

--------------------------------------------------------------------------------
--  MPU6050

-- this is a accelerometer and gyroscope chip with registers
--  * https://randomnerdtutorials.com/arduino-mpu-6050-accelerometer-gyroscope/
--  * https://github.com/adafruit/Adafruit_MPU6050/blob/master/Adafruit_MPU6050.cpp
--  * https://www.invensense.tdk.com/en-us/download-resource/ps-mpu-6000a-00-mpu-6000-and-mpu-6050-datasheet
--  * https://www.invensense.tdk.com/download-resource/rm-mpu-6000a-00-mpu-6000-and-mpu-6050-register-map-and-descriptions

data TEMP_OUT = TEMP_OUT Double

instance Show TEMP_OUT where
    show (TEMP_OUT n) = printf "%+ 3.1f °C" n

-- Temperature in degrees C = (TEMP_OUT Register Value as a signed quantity)/340 + 36.53
instance Storable TEMP_OUT where
    sizeOf a = 2
    alignment a = 1
    peek ptr = do
        n <- peekBE @Int16 $ castPtr ptr
        pure $ TEMP_OUT $ (fromIntegral n) / 340.0 + 36.53
    poke ptr (TEMP_OUT a) = do
        pokeBE @Int16 (castPtr ptr) $ truncate $ (a - 36.53) * 340.0


$(chip "MPU6050" 0x68)

$(register8 "USER_CTRL" 0x6A 0x00)
$(field    ''USER_CTRL "FIFO_RESET"      "00000*00")
$(field    ''USER_CTRL "I2C_MST_RESET"   "000000*0")
$(field    ''USER_CTRL "SIG_COND_RESET"  "0000000*")

$(register8 "PWR_MGMT_1" 0x6B 0x40)
$(field    ''PWR_MGMT_1 "CLKSEL"         "00000***")
$(field    ''PWR_MGMT_1 "SLEEP"          "0*000000")

$(register ''TEMP_OUT 0x41)


testMPU6050 :: IO ()
testMPU6050 = do
    busdev <- openChip @MPU6050 "/dev/i2c-1"

    regwrite busdev $ def & bitsetFIFO_RESET & bitsetI2C_MST_RESET & bitsetSIG_COND_RESET
    regwrite busdev $ def & setCLKSEL 2 & bitclearSLEEP

    forever $ do

        r :: TEMP_OUT <- regread busdev 
        putTextLn $ show $ un @TEMP_OUT r

        threadDelay 400000


------------------------------------------------------------------------------
-- misc helpers
--
printQ :: Show a => Q a -> IO ()
printQ ma = do
    a <- runQ ma
    pPrint a

print8 :: Integral a => a -> IO ()
print8 a = print8' (fromIntegral a :: Word8)

print16 :: Integral a => a -> IO ()
print16 a = print16' (fromIntegral a :: Word16)


print16' :: Coercible Word16 a => a -> IO ()
print16' a = putTextLn $ toText @String $ printf "%016b" $ un @Word16 a

print8' :: Coercible Word8 a => a -> IO ()
print8' a = putTextLn $ toText @String $ printf "%08b" $ un @Word8 a

