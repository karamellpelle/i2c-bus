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

newtype SMALL_REG = SMALL_REG Word8 deriving (Show)
newtype LARGE_REG = LARGE_REG Word16 deriving (Show)

instance Register1 SMALL_REG where
    register1Address = 0xAD
    register1Name = "SMALL_REG"

instance Register2 LARGE_REG where
    register2Address = 0x21
    register2Name = "LARGE_REG"



main :: IO ()
main = do
    busdev <- openChip @Chip123 "/dev/null"

    a <- regread1 @Chip123 busdev
    b <- regread2 @Chip123 busdev

    pPrint $ (a :: SMALL_REG)
    pPrint $ (b :: LARGE_REG)


