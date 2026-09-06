# TODO
* use `mallocBytes` instead of `allocBytes` if transfer size is larger than some value. but how 
  do we free the ptr if some type's `peek`/`poke` throws exceptions?
* check valid name for register (non-empty, starting Uppercase letter)
* for generated `setXXX`: don't use Integral, Num typeclasses but istead underlying Word8, Word16, etc
* what about `HasCallStack` when throwing I2CErr? does `assertOK` give correct info?
