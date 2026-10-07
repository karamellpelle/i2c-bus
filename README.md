# i2c-bus

Library for typed I2C communication with utilities for working with registers. Importing the module `I2C` will give you what you need unless you need access to low level functionality provided by your backend.

## Features
* Connections to chips on I2C buses.
* A module `I2C.Raw` to perform direct write and read using `Storable` types.
* A module `I2C.Register` to work with chips that use byte registers. 
* An exception type `I2CErr`.
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
~~~

See [tests/GHCI.hs](tests/GHCI.hs) and [tests/ssd1306](tests/ssd1306) for more examples. 

![Example](https://github.com/karamellpelle/i2c-bus/blob/main/README/example0.gif?raw=true)
