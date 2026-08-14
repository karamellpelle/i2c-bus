{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -ddump-splices #-}
module GHCI where

import Relude hiding (modify, modify')
import Relude.Extra.Newtype
import I2C
import I2C.TH
import Data.Default
import Text.Show qualified
import Numeric
import Data.Bits
import Data.Char (toUpper)
import Text.Pretty.Simple
import Control.Concurrent

-- # load ghci with linux build:
--    $ stack ghci --package=pretty-simple  --flag=i2c:-build-empty --flag=i2c:build-linux
-- # load ghci with dummy build:
--    $ stack ghci --package=pretty-simple  --flag=i2c:build-empty --flag=i2c:-build-linux
-- then load this file:
--    ghci> :l tests/GHCI.hs
--  
-- (if you get link errors, run `stack test` which will build the FFI parts,
--  and those symbols will then be available in GHCi) 

$(chip "Chip123" 0xDE)


$(register1 "REG_WORD8"  0x10 0x0A)
$(register2 "REG_WORD16" 0x20 0x0B0C)

-- this is a GPIO expander chip with 2 bytes => 16 pins
-- https://www.ti.com/lit/ds/symlink/pcf8575.pdf
$(chip "PCF8575" 0x20)


--fieldSet :: (Bits a, Coercible a r) => (r -> a) 
--fieldSet :: Num w => w -> r -> r
--fieldSet 

main :: IO ()
main = do

#ifdef I2C_INTERNAL_EMPTY
    busdev <- openChip @Chip123 "/dev/i2c-1"

    a <- regread busdev
    b <- regread busdev

    pPrint $ "REG_WORD8:  " <> show (a :: REG_WORD8)
    pPrint $ "REG_WORD16: " <> show (b :: REG_WORD16)

    pPrint $ "Default REG_WORD8:  " <> show (def :: REG_WORD8)
    pPrint $ "Default REG_WORD16: " <> show (def :: REG_WORD16)
#endif


#ifdef I2C_INTERNAL_LINUX
    busdev <- openChip @PCF8575 "/dev/i2c-1"
    
    forM_ [0..0x00FF] $ \ix -> do
        rawwrite @PCF8575 @Word16 busdev ix
        threadDelay 400000
#endif

