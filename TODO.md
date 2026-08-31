# TODO
* clean up test suite
* use same buffer for write and read (allocaBytes)
* add TH for data types 32 and 64 BE/LE
* backend as typeclass so library users can define their own backends, for example 
  from an USB-adapter?
* fix `HasCallStack` when throwing I2CErr; use `withFrozenCallStack`? does `assertOK` give correct info?
