# i2c-bus


Library for typed I2C communication with utilities for working with registers.

## Examples

### Direct write and read

~~~haskell
{-# LANGUAGE TemplateHaskell #-}
import I2C

$(chip "PCF8575")

testPCF8575 :: IO ()
testPCF8575 = do
    busdev <- openChip @PCF8575 "/dev/i2c-1" 0x20
    
    forM_ [0..0x00FF] $ \ix -> do
        rawwrite @Word16LE busdev ix
        threadDelay 400000
~~~

### Working with registers

See [tests/](tests/) for examples.

