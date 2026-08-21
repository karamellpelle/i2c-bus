# TODO
* create associated type synonym (type family) for `class Register`?
* TH: assert that `bitstrToField` does not overflow register size
* go away from linux smbus functions and use more primitive kernel i2c API directly
  (I guess smbus functions are based on more primitive functions). new functions in I2C.Internal.XXX.hs
* move read/write into `foreign.c` for efficiency?
* Linux: set BusDevice options: timeout etc
* fix `HasCallStack` when throwing I2CErr; use `withFrozenCallStack`? does `assertOK` give correct info?
