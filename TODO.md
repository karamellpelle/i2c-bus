# TODO
* use the new `RegisterItem` while implementing functions
* TH: assert that length of `bitstrToField` matches register size
* go away from linux smbus functions and use more primitive kernel i2c API directly
* Linux: set BusDevice options: timeout etc
* fix `HasCallStack` when throwing I2CErr; use `withFrozenCallStack`? does `assertOK` give correct info?
