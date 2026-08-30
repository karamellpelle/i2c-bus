-- Copyright (c) 2026 karamellpelle@hotmail.com
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy of
-- this software and associated documentation files (the "Software"), to deal in
-- the Software without restriction, including without limitation the rights to
-- use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is furnished to do
-- so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
{-# LANGUAGE TemplateHaskell #-}
module I2C.TH
(
  chip,

  register,
  register8,
  register16BE,
  register16LE,

  field,

) where

import Relude hiding (Type)
import Relude.Extra.Newtype
import Data.Default
import Foreign
import Numeric
import Text.Show qualified
import Data.Char (toUpper)
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


--------------------------------------------------------------------------------
--  Chip

-- | declare a Chip with name 'name' at _7_ bit bus address 'addr'
--
--  $(chip "Chip123" 0x22)
--  ======>
--    data Chip123 deriving Show
--    instance Chip Chip123 where
--      chipAddress = 34
--      chipName = "Chip123"
--
chip :: String -> ChipAddress -> Q [Dec]
chip name addr = do
    let name' = mkName name
    dData <- decData name' 
    dInstance <- decInstance name' addr
    pure [dData, dInstance]
    
    where
      decData :: Name -> Q Dec
      decData tname = 
          dataD (cxt []) tname [] Nothing [] $ one $ derivClause Nothing $ [conT $ ''Show] 

      decInstance :: Name -> ChipAddress -> Q Dec
      decInstance tname addr = do
          let dAddress :: Q Dec
              dAddress = funD 'chipAddress $ one $ clause [] (normalB $ litE $ integerL $ fromChipAddress addr) []
              dName :: Q Dec
              dName = funD 'chipName $ one $ clause [] (normalB $ litE $ stringL $ nameBase tname ) []
          instanceD (cxt []) (appT (conT ''Chip) (conT tname)) [dAddress, dName]


--------------------------------------------------------------------------------
--  Register

regread' :: forall chip reg m . (Chip chip, Register reg, Storable reg, MonadIO m) => BusDevice chip -> m reg
regread' busdev = 
    liftIO $ read busdev $ registerAddress @reg

regwrite' :: forall chip reg m . (Chip chip, Register reg, Storable reg, MonadIO m) => BusDevice chip -> reg -> m ()
regwrite' busdev = \r -> 
    liftIO $ write busdev $ StorableAB (registerAddress @reg) r


-- | declare a register of Chip from Storable type. 
--   Storable is relative to I2C chip data
register :: Name -> String -> RegisterAddress -> Q [Dec]
register tywrap name addr = do
    let ty = mkName name
    dNewtype <- decNewtype ty tywrap [''Storable]
    dInstanceRegister <- decInstanceRegister ty addr
    pure [dNewtype, dInstanceRegister]
    
    where
      decInstanceRegister :: Name -> RegisterAddress -> Q Dec
      decInstanceRegister ty addr = do
          let dAddress = funD 'registerAddress $ one $ clause [] (normalB $ litE $ integerL $ fromRegisterAddress addr) []
              dName = funD 'registerName $ one $ clause [] (normalB $ litE $ stringL $ nameBase ty ) []
              dItem = tySynInstD $ tySynEqn Nothing (appT (conT ''RegisterItem) (conT ty)) (conT tywrap)
              dRead = decFunctionAssign 'regread 'regread'
              dWrite = decFunctionAssign 'regwrite 'regwrite'
          instanceD (cxt []) (appT (conT ''Register) (conT ty)) [dAddress, dName, dItem, dRead, dWrite]
    

-- | declare a register of Chip from a numerice type. 
registerN :: Name -> String -> RegisterAddress -> Q [Dec]
registerN tywrap name addr = do
    let ty = mkName name
    dNewtype <- decNewtype ty tywrap [''Storable, ''Eq, ''Num]
    dInstanceRegister <- decInstanceRegister ty addr
    pure [dNewtype, dInstanceRegister]
    
    where
      decInstanceRegister :: Name -> RegisterAddress -> Q Dec
      decInstanceRegister ty addr = do
          let dAddress = funD 'registerAddress $ one $ clause [] (normalB $ litE $ integerL $ fromRegisterAddress addr) []
              dName = funD 'registerName $ one $ clause [] (normalB $ litE $ stringL $ nameBase ty ) []
              dItem = tySynInstD $ tySynEqn Nothing (appT (conT ''RegisterItem) (conT ty)) (conT tywrap)
              dRead = decFunctionAssign 'regread 'regread'
              dWrite = decFunctionAssign 'regwrite 'regwrite'
          instanceD (cxt []) (appT (conT ''Register) (conT ty)) [dAddress, dName, dItem, dRead, dWrite]
    

-- | declare a register of Chip that contains Word8 data
--
--   $(register8 "MYREG8" 0x24 0x11)
--   ======>
--     newtype MYREG8
--       = MYREG8 Store8
--       deriving Storable
--       deriving Eq
--       deriving Num
--     instance Register MYREG8 where
--       type RegisterItem MYREG8 = Store8
--       registerAddress = 36
--       registerName = "MYREG8"
--       regread = I2C.TH.regread'
--       regwrite = I2C.TH.regwrite'
--     instance Default MYREG8 where
--       def = MYREG8 17
--     instance Show MYREG8 where
--       Text.Show.show = I2C.TH.showBin8
--
register8 :: String -> RegisterAddress -> Word8 -> Q [Dec]
register8 name addr def = do
    let name' = mkName name
    dReg <- registerN ''Store8 name addr
    dInstanceDefault <- decInstanceDefault name' def
    dInstanceShow <- decInstanceShow name' 'showBin8
    pure $ dReg <> [dInstanceDefault, dInstanceShow]

-- | declare a register of Chip that contains Word16 data as Little Endian
--
--   $(register16LE "MYREG16" 0x26 0x3322)
--   ======>
--     newtype MYREG16
--       = MYREG16 Store16LE
--       deriving Storable
--       deriving Eq
--       deriving Num
--     instance Register MYREG16 where
--       type RegisterItem MYREG16 = Store16LE
--       registerAddress = 38
--       registerName = "MYREG16"
--       regread = I2C.TH.regread'
--       regwrite = I2C.TH.regwrite'
--     instance Default MYREG16 where
--       def = MYREG16 13090
--     instance Show MYREG16 where
--       Text.Show.show = I2C.TH.showBin16
--
register16LE :: String -> RegisterAddress -> Word16 -> Q [Dec]
register16LE name addr def = do
    let name' = mkName name
    dReg <- registerN ''Store16LE name addr
    dInstanceDefault <- decInstanceDefault name' def
    dInstanceShow <- decInstanceShow name' 'showBin16
    pure $ dReg <> [dInstanceDefault, dInstanceShow]

-- | declare a register of Chip that contains Word16 data as Big Endian
register16BE :: String -> RegisterAddress -> Word16 -> Q [Dec]
register16BE name addr def = do
    let name' = mkName name
    dReg <- registerN ''Store16BE name addr
    dInstanceDefault <- decInstanceDefault name' def
    dInstanceShow <- decInstanceShow name' 'showBin16
    pure $ dReg <> [dInstanceDefault, dInstanceShow]



--------------------------------------------------------------------------------
--  fields

-- | declare a (sub)field of a register 
--
--   $(field ''MYREG8 "VALUES" "00***000")
--   ======>
--     getVALUES :: Integral n => MYREG8 -> n
--     getVALUES
--       = \w -> (fromIntegral $ (unsafeShiftR (un @Store8 w) 3 .&. 7))
--     setVALUES :: Integral n => n -> MYREG8 -> MYREG8
--     setVALUES
--       = \ n -> (under @Store8 $ (\ w -> ((w .&. complement 56) .|. unsafeShiftL (7 .&. fromIntegral n) 3)))
--
--   $(field ''MYREG16 "ENABLE" "000*000000000000")
--   ======>
--     getENABLE :: Integral n => MYREG16 -> n
--     getENABLE
--       = \w -> (fromIntegral $ (unsafeShiftR (un @Store16LE w) 12 .&. 1))
--     setENABLE :: Integral n => n -> MYREG16 -> MYREG16
--     setENABLE
--       = \ n -> (under @Store16LE $ (\ w -> ((w .&. complement 4096) .|. unsafeShiftL (1 .&. fromIntegral n) 12)))
--     bitsetENABLE :: MYREG16 -> MYREG16
--     bitsetENABLE = under @Store16LE (flip setBit 12)
--     bitclearENABLE :: MYREG16 -> MYREG16
--     bitclearENABLE = under @Store16LE (flip clearBit 12)
--     bittoggleENABLE :: MYREG16 -> MYREG16
--     bittoggleENABLE = under @Store16LE (flip complementBit 12)
--
field :: Name -> String -> String -> Q [Dec]
field ty name bitstr = case bitstrToField bitstr of
    Left err              -> fail err
    Right sil@(size, ix, len) -> do
        info <- reify ty
        runIO $ print info
  
        TyConI (NewtypeD _ _ty _ _ (NormalC tycon [(_, ConT tywrap)]) _)  <- reify ty
        TyConI (NewtypeD _ _ty _ _ (NormalC tycon [(_, ConT tywrap')]) _)  <- reify tywrap

        info <- reify tywrap
        runIO $ print info
        
        assertCorrectSize size tywrap'
        dGet <- decGet tywrap ty tycon sil
        dSet <- decSet tywrap ty tycon sil
        dBit <- if len == 1 then decBit tywrap ty sil else mempty
        pure $ dGet <> dSet <> dBit
    where
        assertCorrectSize size ty | ty == ''Word8   = when (size /= 8)  $ fail $ "Bitstring " <> bitstr <> " doesn't match expected size 8 (got "  <> show size <> ")"
                                  | ty == ''Word16  = when (size /= 16) $ fail $ "Bitstring " <> bitstr <> " doesn't match expected size 16 (got " <> show size <> ")"
                                  | ty == ''Word32  = when (size /= 32) $ fail $ "Bitstring " <> bitstr <> " doesn't match expected size 32 (got " <> show size <> ")"
                                  | ty == ''Word64  = when (size /= 64) $ fail $ "Bitstring " <> bitstr <> " doesn't match expected size 64 (got " <> show size <> ")"
                                  | otherwise       = fail $ "Didn't expect type " <> show ty

        decGet tywrap ty tycon (size, ix, len) = do
            let funname = mkFunctionName $ "get" <> name  
                maskE = LitE $ IntegerL $ mkMaskN len 
            n <- newName "n" 
            w <- newName "w" 

            pure  [ SigD funname (ForallT [] [AppT (ConT ''Integral) (VarT n)] (AppT (AppT ArrowT (ConT ty)) (VarT n)))
                  , ValD (VarP funname) (NormalB (LamE [VarP w] (InfixE (Just (VarE 'fromIntegral)) (VarE '($)) (Just (InfixE (Just (AppE (AppE (VarE 'unsafeShiftR) 
                  (AppE (AppTypeE (VarE 'un) (ConT tywrap)) (VarE w))) (LitE (IntegerL $ fromIntegral ix)))) (VarE '(.&.)) (Just maskE)))))) []
                  ]

        decSet tywrap ty tycon (size, ix, len) = do
            let funname = mkFunctionName $ "set" <> name  
                maskE0 = LitE $ IntegerL $ mkMaskIxLen ix len 
                maskE1 = LitE $ IntegerL $ mkMaskN len 
                ixE    = LitE $ IntegerL $ fromIntegral ix
            n <- newName "n" 
            w <- newName "w" 
            
            pure  [ SigD funname (ForallT [] [AppT (ConT ''Integral) (VarT n)] (AppT (AppT ArrowT (VarT n)) (AppT (AppT ArrowT (ConT ty)) (ConT ty))))
                  , ValD (VarP funname) (NormalB (LamE [VarP n] (InfixE (Just (AppTypeE (VarE 'under) (ConT tywrap))) (VarE '($)) (Just (LamE [VarP w] (InfixE (Just (InfixE (Just (VarE w)) (VarE '(.&.)) (Just (AppE (VarE 'complement) (maskE0))))) (VarE '(.|.)) (Just (AppE (AppE (VarE 'unsafeShiftL) (InfixE (Just maskE1) (VarE '(.&.)) (Just (AppE (VarE 'fromIntegral) (VarE n))))) ixE)))))))) []
                  ]

        decBit tywrap ty (size, ix, len) = do
            fmap concat $ forM [("bitset", 'setBit), ("bitclear", 'clearBit), ("bittoggle", 'complementBit)] $ \(prefix, underF) -> do
                let funname = mkFunctionName $ prefix <> name  
                    ixE     = LitE $ IntegerL $ fromIntegral ix
                pure  [ SigD funname (AppT (AppT ArrowT (ConT ty)) (ConT ty))
                      , ValD (VarP funname) (NormalB (AppE (AppTypeE (VarE 'under) (ConT tywrap)) (AppE (AppE (VarE 'flip) (VarE underF)) ixE)))
                      []]

-- 3 => 0b0111
mkMaskN :: Word -> Integer
mkMaskN n = 
    shiftL 0b1 (fromIntegral n) - 1

mkMaskIxLen :: Word -> Word -> Integer
mkMaskIxLen ix len = 
    shiftL (shiftL 0b1 (fromIntegral len) - 1) (fromIntegral ix)

mkFunctionName :: String -> Name
mkFunctionName = mkName 

--------------------------------------------------------------------------------
--  helpers

decInstanceDefault :: Integral value => Name -> value -> Q Dec
decInstanceDefault tname v = do
    let dDef :: Q Dec
        dDef = funD 'def $ one $ clause [] (normalB $ appE (conE tname) (litE $ integerL $ toInteger v)) []
    instanceD (cxt []) (appT (conT ''Default) (conT tname)) [dDef]

decInstanceShow :: Name -> Name -> Q Dec
decInstanceShow ty f =
    instanceD (cxt []) (appT (conT ''Show) (conT ty)) $ one $ decFunctionAssign 'Text.Show.show f

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
          []        -> rest ix ix ix []
      count ix len as = case as of
          ('*':as') -> count ix (succ len) as' 
          (_:as')   -> rest ix len (ix + len) as
          []        -> rest ix len (ix + len) []
      rest ix len total as = case as of
          ('*':_)   -> Left $ "field is disconnected: " <> bitstr
          (_:as')   -> rest ix len (succ total) as'
          []        -> Right (total, ix, len)


--------------------------------------------------------------------------------
--  shows

showHex8 :: forall reg . (Register reg, Coercible Word8 reg) => reg -> String
showHex8 reg = 
    (toString $ registerName @reg) <> "(" <> showWordN 2 (un @Word8 reg) <> ")"

showHex16 :: forall reg . (Register reg, Coercible Word16 reg) => reg -> String
showHex16 reg = 
    (toString $ registerName @reg) <> "(" <> showWordN 4 (un @Word16 reg) <> ")"

showHex32 :: forall reg . (Register reg, Coercible Word32 reg) => reg -> String
showHex32 reg = 
    (toString $ registerName @reg) <> "(" <> showWordN 8 (un @Word32 reg) <> ")"

showHex64 :: forall reg . (Register reg, Coercible Word64 reg) => reg -> String
showHex64 reg = 
    (toString $ registerName @reg) <> "(" <> showWordN 16 (un @Word64 reg) <> ")"

showBin8 :: forall reg . (Register reg, Coercible Word8 reg) => reg -> String
showBin8 reg = 
    (toString $ registerName @reg) <> "(" <> showBinN 8 (un @Word8 reg) <> ")"

showBin16 :: forall reg . (Register reg, Coercible Word16 reg) => reg -> String
showBin16 reg = 
    (toString $ registerName @reg) <> "(" <> showBinN 16 (un @Word16 reg) <> ")"

showBin32 :: forall reg . (Register reg, Coercible Word32 reg) => reg -> String
showBin32 reg = 
    (toString $ registerName @reg) <> "(" <> showBinN 32 (un @Word32 reg) <> ")"

showBin64 :: forall reg . (Register reg, Coercible Word64 reg) => reg -> String
showBin64 reg = 
    (toString $ registerName @reg) <> "(" <> showBinN 64 (un @Word64 reg) <> ")"


showWordN :: Integral a => Int -> a -> String
showWordN n w =
    let str = fmap toUpper $ showHex w ""
        n'  = if length str <= n then n - length str else 0
    in replicate n' '0' <> str

showBinN :: Integral a => Int -> a -> String
showBinN n w =
    let str = showBin w ""
        n'  = if length str <= n then n - length str else 0
    in replicate n' '0' <> str

