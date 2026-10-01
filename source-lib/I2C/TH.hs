--------------------------------------------------------------------------------
-- | 
-- Module                  : I2C.TH
-- SPDX-License-Identifier : MIT
-- Copyright               : karamellpelle@hotmail.com
-- Maintainer              : karamellpelle@hotmail.com
-- Stability               : experimental
--
-- Create chips, registers and fields easily using Template Haskell.
--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module I2C.TH
(
    -- * Create Chips
    chip,

    -- * Create Registers
    register,
    register8,
    -- ** Little endian
    register16LE,
    register32LE,
    register64LE,
    -- ** Big endian
    register16BE,
    register32BE,
    register64BE,

    -- * Create fields
    field,

    -- * Imperative settings
    -- $settings
    --
    setDefaults,
    setPrefixRegister,
    setShowBinary,
    setShowHex,

) where

import Relude hiding (Type)
import Relude.Extra.Newtype
import Data.Default
import Foreign
import Numeric
import Text.Show qualified
import Data.Char
import Data.Bits

import I2C.Types
import I2C.Chip
import I2C.Internal
import I2C.Register
import I2C.Raw

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib


--------------------------------------------------------------------------------
--  create Chips and Registers through Template Haskell !
--------------------------------------------------------------------------------

-- $settings
--
-- Settings that control code generation. A new setting only applies to 
-- the TH calls that follows, hence you can have different settings for different
-- calls.
-- 

data QSetting = QSetting {
                qsettingPrefixRegister :: String 
              , qsettingShowVariant :: ShowVariant
              }

instance Default QSetting where
    def = QSetting {
          qsettingPrefixRegister = "reg"
        , qsettingShowVariant = ShowHex
        }


-- | Restore to default settings
setDefaults :: String -> Q [Dec]
setDefaults pre = do
    putQ @QSetting def 
    pure []

-- | Set prefix for declared Register values. Default prefix is @reg@.
setPrefixRegister :: String -> Q [Dec]
setPrefixRegister pre = do
    assertNamePrefix pre
    getQ >>= \case 
        Nothing  -> putQ $ def { qsettingPrefixRegister = pre }
        Just set -> putQ $ set { qsettingPrefixRegister = pre }

    pure mempty
    where
      assertNamePrefix name = case name of
        ""      -> fail "Register prefixes must be non-empty"
        (c:cs)  -> do
            -- first make sure we restrict characters to ASCII
            when (not $ all isAscii name) $ fail "Invalid characters in register prefix (non-ASCII)"

            when (not $ isAsciiLower c) $ fail "Register prefixes must at start with lowercase [a-z]"
            when (not $ all (\c -> isAlphaNum c || c == '_' || c == '\'') name) $ fail "Register prefixes must only contain alphanums, '_' or '\\''"

data ShowVariant = ShowBin
                 | ShowHex

-- | Implement instance Show as binary string
--
--   Example: 
--
--   >>> show my8
--   >>> "MY8(00000110)"
--
setShowBinary :: Q [Dec]
setShowBinary = do
    getQ >>= \case 
        Nothing  -> putQ $ def { qsettingShowVariant = ShowBin }
        Just set -> putQ $ set { qsettingShowVariant = ShowBin }
    pure mempty

-- | Implement instance Show as hex string.
--
--   Example: 
--
--   >>> show my16 
--   >>> "MY16(0F18)"
--
setShowHex :: Q [Dec]
setShowHex = do
    getQ >>= \case 
        Nothing  -> putQ $ def { qsettingShowVariant = ShowHex }
        Just set -> putQ $ set { qsettingShowVariant = ShowHex }
    pure mempty 

getShowVariant :: Q ShowVariant
getShowVariant = 
   getQ >>= \case 
       Nothing  -> pure $ qsettingShowVariant def
       Just set -> pure $ qsettingShowVariant set

--------------------------------------------------------------------------------
--  Chip

-- | Declare a 'Chip' from name
--
-- > $(chip "MYCHIP")
-- > ======>
-- >   data MYCHIP deriving Show
-- >   instance Chip MYCHIP where
-- >     chipName = "MYCHIP"
--
chip :: String -> -- ^ Name of data type
        Q [Dec]
chip name = do
    let name' = mkName name
    dData <- decData name' 
    dInstance <- instanceChip name' 
    pure $ dData <> dInstance
    
    where
      decData :: Name -> Q [Dec]
      decData tname = 
          fmap one $ dataD (cxt []) tname [] Nothing [] $ one $ derivClause Nothing $ [conT $ ''Show] 

instanceChip :: Name -> Q [Dec]
instanceChip ty = do
    let dName :: Q Dec
        dName = funD 'chipName $ one $ clause [] (normalB $ litE $ stringL $ nameBase ty ) []
    fmap one $ instanceD (cxt []) (appT (conT ''Chip) (conT ty)) [dName]


--------------------------------------------------------------------------------
--  Register


-- | Declare a 'Register' of a chip. 
--   The data type of the register has to implement 'Storable',
--   and this implemetation is relative to the chip's hardware.
--
-- > $(register ''MPU6050 0x41 "TEMP_OUT" ''TemperatureC)
-- > ======>
-- >   regTEMP_OUT :: Register MPU6050 TemperatureC
-- >   regTEMP_OUT = Register "TEMP_OUT" 65
--
register :: Name ->             -- ^ Chip this register belongs to
            RegisterAddress ->  -- ^ Register address
            String ->           -- ^ Register name
            Name ->             -- ^ Type of data in this register. Must be an instance of 'Storable'.
            Q [Dec]
register tychip addr name ty = do
    assertNameRegister name
    regname <- mkNameRegister name
    pure  [ SigD regname (AppT (AppT (ConT ''Register) (ConT tychip)) (ConT ty))
          , ValD (VarP regname) (NormalB (AppE (AppE (ConE 'Register) (LitE (StringL name))) (LitE (IntegerL $ fromRegisterAddress addr)))) []
          ]


-- | Declare a register of Chip with custom type wrapping Word8 data.
--
-- > $(register8 ''MYCHIP 0x22 "MY8" 0x83)
-- > ======>
-- >   newtype MY8
-- >     = MY8 Word8
-- >     deriving Storable
-- >     deriving Eq
-- >   instance Default MY8 where
-- >     def = MY8 131
-- >   instance Show MY8 where
-- >     Text.Show.show = I2C.TH.showRegT8Bin "MY8"
-- >   regMY8 :: Register MYCHIP MY8
-- >   regMY8 = Register "MY8" 34
register8 :: Name ->            -- ^ Chip this register belongs to
             RegisterAddress -> -- ^ Register address on chip      
             String ->          -- ^ Register name                 
             Word8 ->           -- ^ Default value (if any)
             Q [Dec]
register8 tychip addr name def = do
    showv <- getShowVariant
    registerN tychip addr name def ''Word8 $ case showv of
        ShowHex -> 'showRegT8Hex
        ShowBin -> 'showRegT8Bin
 

-- | Declare a register of Chip with custom type wrapping Word16 as Little Endian.
register16LE :: Name -> RegisterAddress -> String -> Word16 -> Q [Dec]
register16LE tychip addr name def = do
    showv <- getShowVariant
    registerN tychip addr name def ''Word16LE $ case showv of
        ShowHex -> 'showRegT16Hex
        ShowBin -> 'showRegT16Bin

-- | Declare a register of Chip with custom type wrapping Word16 as Big Endian
register16BE :: Name -> RegisterAddress -> String -> Word16 -> Q [Dec]
register16BE tychip addr name def = do
    showv <- getShowVariant
    registerN tychip addr name def ''Word16BE $ case showv of
        ShowHex -> 'showRegT16Hex
        ShowBin -> 'showRegT16Bin

-- | Declare a register of Chip with custom type wrapping Word32 as Little Endian
register32LE :: Name -> RegisterAddress -> String -> Word32 -> Q [Dec]
register32LE tychip addr name def = do
    showv <- getShowVariant
    registerN tychip addr name def ''Word32LE $ case showv of
        ShowHex -> 'showRegT32Hex
        ShowBin -> 'showRegT32Bin

-- | Declare a register of Chip with custom type wrapping Word32 as Big Endian
register32BE :: Name -> RegisterAddress -> String -> Word32 -> Q [Dec]
register32BE tychip addr name def = do
    showv <- getShowVariant
    registerN tychip addr name def ''Word32BE $ case showv of
        ShowHex -> 'showRegT32Hex
        ShowBin -> 'showRegT32Bin

-- | Declare a register of Chip with custom type wrapping Word64 as Little Endian
register64LE :: Name -> RegisterAddress -> String -> Word64 -> Q [Dec]
register64LE tychip addr name def = do
    showv <- getShowVariant
    registerN tychip addr name def ''Word64LE $ case showv of
        ShowHex -> 'showRegT64Hex
        ShowBin -> 'showRegT64Bin

-- | Declare a register of Chip with custom type wrapping Word64 as Big Endian
register64BE :: Name -> RegisterAddress -> String -> Word64 -> Q [Dec]
register64BE tychip addr name def = do
    showv <- getShowVariant
    registerN tychip addr name def ''Word64BE $ case showv of
        ShowHex -> 'showRegT64Hex
        ShowBin -> 'showRegT64Bin


registerN :: Integral n => Name -> RegisterAddress -> String -> n -> Name -> Name -> Q [Dec]
registerN tychip addr name def tywrap showf = do
    assertNameRegister name
    ty <- mkNameType name
    dNewtype <- decNewtype ty tywrap [''Storable, ''Eq]
    dInstanceDefault <- decInstanceDefault ty def
    dInstanceShow <- decInstanceShowRegT name ty showf
    dRegister <- register tychip addr name ty
    pure $ [dNewtype, dInstanceDefault, dInstanceShow] <> dRegister


-- | see 2.4 Identifiers and Operators: https://www.haskell.org/onlinereport/lexemes.html
assertNameRegister :: String -> Q ()
assertNameRegister name = case name of
    ""      -> fail "Register names must be non-empty"
    (c:cs)  -> do
        -- first make sure we restrict characters to ASCII
        when (not $ all isAscii name) $ fail "Invalid characters in Register name (non-ASCII)"

        when (not $ isAsciiUpper c) $ fail "Register names must start with uppercase [A-Z]"
        when (not $ all (\c -> isAlphaNum c || c == '_' || c == '\'') name) $ fail "Register names must only contain alphanums, '_' or '\\''"

--------------------------------------------------------------------------------
--  fields

-- | Define a data type inside a register type. The subset is defined by a string 
-- having the same length as the bitsize of the register, wherein the  @*@ characters
-- defines the field (other characters are considered placeholders).
--
-- If the subset is a 1 bit set, then additional code for bit manipulation will be 
-- generated too.
--
--
-- > $(field ''MYREG8 "VALUES" "00***000")
-- > ======>
-- >   getVALUES :: MYREG8 -> Word8
-- >   getVALUES
-- >     = \w -> (un @Word8 $ (unsafeShiftR (un @Word8 w) 3 .&. 7))
-- >   setVALUES :: Word8 -> MYREG8 -> MYREG8
-- >   setVALUES
-- >     = \n -> (under @Word8 $ (\w -> ((w .&. complement 56) .|. unsafeShiftL (7 .&. wrap @Word8 n) 3)))
-- >
-- > $(field ''MYREG16 "ENABLE" "000*000000000000")
-- > ======>
-- >   getENABLE :: MYREG16 -> Word16
-- >   getENABLE
-- >     = \w -> (un @Word16 $ (unsafeShiftR (un @Word16LE w) 12 .&. 1))
-- >   setENABLE :: Word16 -> MYREG16 -> MYREG16
-- >   setENABLE
-- >     = \n -> (under @Word16LE $ (\w -> ((w .&. complement 4096) .|. unsafeShiftL (1 .&. wrap @Word16LE n) 12)))
-- >   bitsetENABLE :: MYREG16 -> MYREG16
-- >   bitsetENABLE = under @Word16LE (flip setBit 12)
-- >   bitclearENABLE :: MYREG16 -> MYREG16
-- >   bitclearENABLE = under @Word16LE (flip clearBit 12)
-- >   bittoggleENABLE :: MYREG16 -> MYREG16
-- >   bittoggleENABLE = under @Word16LE (flip complementBit 12)
--
field :: Name ->      -- ^ Which register type the field is contained in
         String ->    -- ^ Name of field
         String ->    -- ^ String that defines the field.
         Q [Dec]
field ty name bitstr = case bitstrToField bitstr of
    Left err              -> fail err
    Right sil@(size, ix, len) -> do
        --info <- reify ty
        --runIO $ print info
  
        TyConI (NewtypeD _ _ty _ _ (NormalC tycon [(_, ConT tywrap)]) _)  <- reify ty
        info <- reify tywrap
        let tywrap' = case info of
                TyConI (NewtypeD _ _ty _ _ (NormalC tycon [(_, ConT tywrap')]) _)   -> tywrap' -- if wrapped inside WordXXLE/WordXXBE
                _                                                                   -> tywrap  -- if not wrapped, i.e. Word8

        assertCorrectSize size tywrap'

        dGet <- decGet tywrap' tywrap ty tycon sil
        dSet <- decSet tywrap' tywrap ty tycon sil
        dBit <- if len == 1 then decBit tywrap ty sil else mempty

        pure $ dGet <> dSet <> dBit

    where
        assertCorrectSize size ty | ty == ''Word8   = when (size /= 8)  $ fail $ "Bitstring " <> bitstr <> " doesn't match expected size 8 (got "  <> show size <> ")"
                                  | ty == ''Word16  = when (size /= 16) $ fail $ "Bitstring " <> bitstr <> " doesn't match expected size 16 (got " <> show size <> ")"
                                  | ty == ''Word32  = when (size /= 32) $ fail $ "Bitstring " <> bitstr <> " doesn't match expected size 32 (got " <> show size <> ")"
                                  | ty == ''Word64  = when (size /= 64) $ fail $ "Bitstring " <> bitstr <> " doesn't match expected size 64 (got " <> show size <> ")"
                                  | otherwise       = fail $ "Didn't expect type " <> show ty

        decGet tywrap' tywrap ty tycon (size, ix, len) = do
            let funname = mkFunctionName $ "get" <> name  
                maskE = LitE $ IntegerL $ mkMaskN len 
            n <- newName "n" 
            w <- newName "w" 

            pure  [ SigD funname (AppT (AppT ArrowT (ConT ty)) (ConT tywrap'))
                  , ValD (VarP funname) (NormalB (LamE [VarP w] (InfixE (Just (AppTypeE (VarE 'un) (ConT tywrap'))) (VarE '($)) (Just (InfixE (Just (AppE (AppE (VarE 'unsafeShiftR) (AppE (AppTypeE (VarE 'un) (ConT tywrap)) (VarE w))) (LitE (IntegerL $ fromIntegral ix)))) (VarE '(.&.)) (Just maskE)))))) []
                  ]

        decSet tywrap' tywrap ty tycon (size, ix, len) = do
            let funname = mkFunctionName $ "set" <> name  
                maskE0 = LitE $ IntegerL $ mkMaskIxLen ix len 
                maskE1 = LitE $ IntegerL $ mkMaskN len 
                ixE    = LitE $ IntegerL $ fromIntegral ix
            n <- newName "n" 
            w <- newName "w" 
            
            pure  [ SigD funname (AppT (AppT ArrowT (ConT tywrap')) (AppT (AppT ArrowT (ConT ty)) (ConT ty)))
                  , ValD (VarP funname) (NormalB (LamE [VarP n] (InfixE (Just (AppTypeE (VarE 'under) (ConT tywrap))) (VarE '($)) (Just (LamE [VarP w] (InfixE (Just (InfixE (Just (VarE w)) (VarE '(.&.)) (Just (AppE (VarE 'complement) (maskE0))))) (VarE '(.|.)) (Just (AppE (AppE (VarE 'unsafeShiftL) (InfixE (Just maskE1) (VarE '(.&.)) (Just (AppE (AppTypeE (VarE 'wrap) (ConT tywrap)) (VarE n))))) ixE)))))))) []
                  ]

        decBit tywrap ty (size, ix, len) = do
            fmap concat $ forM [("bitset", 'setBit), ("bitclear", 'clearBit), ("bittoggle", 'complementBit)] $ \(prefix, underF) -> do
                let funname = mkFunctionName $ prefix <> name  
                    ixE     = LitE $ IntegerL $ fromIntegral ix
                pure  [ SigD funname (AppT (AppT ArrowT (ConT ty)) (ConT ty))
                      , ValD (VarP funname) (NormalB (AppE (AppTypeE (VarE 'under) (ConT tywrap)) (AppE (AppE (VarE 'flip) (VarE underF)) ixE)))
                      []]


--------------------------------------------------------------------------------
--  helpers

-- example: 3 -> 0b00000111
mkMaskN :: Word -> Integer
mkMaskN n = 
    shiftL 0b1 (fromIntegral n) - 1

-- example: 1 3 -> 0b00001110 
mkMaskIxLen :: Word -> Word -> Integer
mkMaskIxLen ix len = 
    shiftL (shiftL 0b1 (fromIntegral len) - 1) (fromIntegral ix)

mkFunctionName :: String -> Name
mkFunctionName = mkName 

mkNameRegister :: String -> Q Name
mkNameRegister name = do
    setting <- fmap (fromMaybe def) $ getQ @QSetting
    pure $ mkName $ qsettingPrefixRegister setting <> name

mkNameType :: String -> Q Name
mkNameType name = 
    pure $ mkName $ name


decInstanceDefault :: Integral value => Name -> value -> Q Dec
decInstanceDefault tname v = do
    let dDef :: Q Dec
        dDef = funD 'def $ one $ clause [] (normalB $ appE (conE tname) (litE $ integerL $ toInteger v)) []
    instanceD (cxt []) (appT (conT ''Default) (conT tname)) [dDef]

decInstanceShowRegT :: String -> Name -> Name -> Q Dec
decInstanceShowRegT name ty showf =
    pure $ InstanceD Nothing [] (AppT (ConT ''Text.Show.Show) (ConT ty)) [ValD (VarP 'Text.Show.show) (NormalB (AppE (VarE showf) (LitE (StringL name)))) []]

decNewtype :: Name -> Name -> [Name] -> Q Dec
decNewtype tname tname' tderivs = 
    newtypeD (cxt []) tname [] Nothing (normalC' tname [conT tname']) $ fmap derive tderivs
    where
      derive :: Name -> Q DerivClause
      derive tname = derivClause Nothing $ [conT $ tname] 

normalC' :: Quote m => Name -> [m Type] -> m Con
normalC' tname ts =
    normalC tname (fmap (fmap helper) ts)
    where
      helper :: Type -> BangType
      helper = \tp -> (Bang NoSourceUnpackedness NoSourceStrictness, tp)

decFunctionAssign :: Name -> Name -> Q Dec
decFunctionAssign a b = 
    funD a $ one $ clause [] (normalB $ varE b) []


-- | bitstring to (size, index, length). 
--   bitstring read as big endian since that is how bits and bytes are 
--   written in programming syntax. example: "00000***0" -> Right (9, 1, 3)
bitstrToField :: String -> Either String (Word, Word, Word) 
bitstrToField bitstr = 
    strip 0 $ reverse bitstr
    where
      strip ix as = case as of
          ('*':as') -> count ix 0 as
          (_:as')   -> strip (succ ix) as'
          []        -> Left $ "Field is empty, no * marks at all: " <> bitstr
      count ix len as = case as of
          ('*':as') -> count ix (succ len) as' 
          (_:as')   -> rest ix len (ix + len) as
          []        -> rest ix len (ix + len) []
      rest ix len total as = case as of
          ('*':_)   -> Left $ "Field is disconnected: " <> bitstr
          (_:as')   -> rest ix len (succ total) as'
          []        -> Right (total, ix, len)


--------------------------------------------------------------------------------
--  shows

showRegT8Bin :: (Coercible Word8 a) => String -> a -> String
showRegT8Bin name a =
    name <> "(" <> showBinN 8 (un @Word8 a) <> ")"

showRegT16Bin :: (Coercible Word16 a) => String -> a -> String
showRegT16Bin name a =
    name <> "(" <> showBinN 16 (un @Word16 a) <> ")"
    
showRegT32Bin :: (Coercible Word32 a) => String -> a -> String
showRegT32Bin name a =
    name <> "(" <> showBinN 32 (un @Word32 a) <> ")"
    
showRegT64Bin :: (Coercible Word64 a) => String -> a -> String
showRegT64Bin name a =
    name <> "(" <> showBinN 64 (un @Word64 a) <> ")"
    
    
showRegT8Hex :: (Coercible Word8 a) => String -> a -> String
showRegT8Hex name a =
    name <> "(" <> showHexN 2 (un @Word8 a) <> ")"
    
showRegT16Hex :: (Coercible Word16 a) => String -> a -> String
showRegT16Hex name a =
    name <> "(" <> showHexN 4 (un @Word16 a) <> ")"
    
showRegT32Hex :: (Coercible Word32 a) => String -> a -> String
showRegT32Hex name a =
    name <> "(" <> showHexN 8 (un @Word32 a) <> ")"

showRegT64Hex :: (Coercible Word64 a) => String -> a -> String
showRegT64Hex name a =
    name <> "(" <> showHexN 16 (un @Word64 a) <> ")"
    

    
showHexN :: Integral a => Int -> a -> String
showHexN n w =
    let str = fmap toUpper $ showHex w ""
        n'  = if length str <= n then n - length str else 0
    in replicate n' '0' <> str

showBinN :: Integral a => Int -> a -> String
showBinN n w =
    let str = showBin w ""
        n'  = if length str <= n then n - length str else 0
    in replicate n' '0' <> str

