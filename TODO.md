# TODO
* backend as typeclass so library users can define their own backends, for example 
  from an USB-adapter?
* TH: assert that length of `bitstrToField` matches register size: look at wrapped type and compare accordingly
* Linux: set BusDevice options: timeout etc
* fix `HasCallStack` when throwing I2CErr; use `withFrozenCallStack`? does `assertOK` give correct info?
