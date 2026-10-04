# TODO
* add I2C.EEPROM module?
* add I2C.SMBus module? especially the "block" type is special for SMBus: 1 byte for size (max 32 (?)) + following data bytes
* Is the `IsChip` constraint a bit unnecessary? This typeclass is only used for a custom `Show`, otherwise no functions use that property.
  If this typeclass would be populated more, what functionality could that be? Manufacturer name? Capabilities? Could this type be used 
  as data for the type `Chip`? 
* Should `Storable a b` be UNPACKed?
