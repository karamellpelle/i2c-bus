# i2c-bus

Library for typed I2C communication with utilities for working with registers. Importing the module `I2C` will give you what you need unless you need access to low level functionality provided by your backend.

## Features
* Connections to chips on I2C buses.
* An exception type `I2CErr`.
* A module `I2C.Raw` to perform direct write and read using `Storable` types.
* A module `I2C.Register` to work with chips that use registers. 
* Types for binary serialization between host architecture and chip hardware.
* Handy utilities through Template Haskell to define chips, registers and (sub)fields of registers.
* Settings to control the code generation by Template Haskell.
* An API specification for backend implementations. Currently, there is only a Linux backend implemented.
* `safe` FFI imports so that the calls do not block [Haskell concurrency](https://www.vex.net/~trebla/haskell/ghc-conc-ffi.xhtml).

## Examples

~~~haskell
{-# LANGUAGE TemplateHaskell #-}
import I2C

$(chip "PCF8575")

testPCF8575 :: IO ()
testPCF8575 = do
    chip <- openChip @PCF8575 "/dev/i2c-1" 0x20
    
    forM_ [0..0x00FF] $ \ix -> do
        rawwrite @Word16LE chip ix
        threadDelay 400000


$(chip "MYCHIP")

$(register8 ''MYCHIP 0x22 "MY8" 0x83)
-- ^ a register of type 'Word8' named MY8 at address 0x22 with default 
--   value 0x83. this generates a type 'MY8' and a value 
--   'regMY8 :: Register MYCHIP MY8'

$(field ''MY8   "A_FIELD"  "0000***0")
-- ^ MY8 contains a 3 bit (sub)field named A_FIELD. this generates a 
--   type 'A_FIELD' with get and set functions over the 'MY8' type.

$(field ''MY8   "A_BIT"    "00*00000")
-- ^ MY8 also has a one bit field named A_BIT. this generates a type
--   'A_BIT' with get and set functions over the 'MY8' type, and 
--   additional functions for bit manipulation (bitset, bitclear, bittoggle).

$(register ''MYCHIP 0x33 "MY_A" ''A)
-- ^ a register of type 'A' named MY_A at address 0x33. 
--   this generates a value 'regMY_A :: Register MYCHIP A'. the type
--   'A' must be an instance of 'Storable', and the Storable implementation 
--   is relative to the chip hardware.
~~~

See [tests/GHCI.hs](tests/GHCI.hs) and [tests/ssd1306](tests/ssd1306) for more examples. 

![Example](https://github.com/karamellpelle/i2c-bus/blob/main/README/example0.gif?raw=true)
