{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module GHCI where

import Relude hiding (modify, modify')
import Relude.Extra.Newtype
import I2C
import Data.Default
import Text.Show qualified
import Numeric
import Data.Bits
import Data.Char (toUpper)
import Text.Pretty.Simple

-- # load ghci with linux build:
--    $ stack ghci --package=pretty-simple  --flag=i2c:-build-empty --flag=i2c:build-linux
-- # load ghci with linux build:
--    $ stack ghci --package=pretty-simple  --flag=i2c:build-empty --flag=i2c:-build-linux
-- then load this file:
--    ghci> :l tests/GHCI.hs
--  
-- (if you get link errors, run `stack test` which will build the FFI parts,
--  and those symbols will then be retrieved by GHCi) 

$(chip "Chip123" 0xDE)

$(register1 "REG_WORD8"  0x10 0x0A)
$(register2 "REG_WORD16" 0x20 0x0B0C)


main :: IO ()
main = do
    busdev <- openChip @Chip123 "/dev/null"

    a <- regread1 busdev
    b <- regread2 busdev

    pPrint $ "REG_WORD8:  " <> show (a :: REG_WORD8)
    pPrint $ "REG_WORD16: " <> show (b :: REG_WORD16)

    pPrint $ "Default REG_WORD8:  " <> show (def :: REG_WORD8)
    pPrint $ "Default REG_WORD16: " <> show (def :: REG_WORD16)


