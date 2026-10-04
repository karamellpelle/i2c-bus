# TODO
* add I2C.EEPROM module?
* add I2C.SMBus module? especially the "block" type is special for SMBus: 1 byte for size (max 32 (?)) + following data bytes
* Is the `Chip` constraint unnecessary? This typeclass is only used for a custom `Show`, and only used in `openChip`.
  If this typeclass would be populated more, what functionality could that be?
* Should `Storable a b` be UNPACKed?
