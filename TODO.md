# TODO
* go away from linux smbus functions and use more primitive i2c functions directly 
  (I guess smbus functions are based on more primitive functions)
* move read/write into `foreign.c` for efficiency?
* Linux: set BusDevice options: timeout etc
* Template Haskell functions
* fix `HasCallStack` when throwing I2CErr. use `withFrozenCallStack`?
