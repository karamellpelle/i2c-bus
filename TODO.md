# TODO
* Use `Store16LE` in example code
* Use `MonadIO` on open/close?
* add I2C.EEPROM module?
* add I2C.SMBus module? especially the `block` type: size byte + data bytes
* TH: add setting that defines generated show instance (binary or hex) 
* TH: assert valid register name is non-empty and starts with Uppercase letter
* main module I2C redefines `openChip/closeChip` using `MonadIO`?
* Move `RegisterAddress` from `I2C.Types` into `I2C.Register`
