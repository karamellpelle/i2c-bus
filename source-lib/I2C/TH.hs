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
  register1,
  register2,

  field,

) where

import Relude hiding (Type)
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
--  create Chips and Registers through Template Haskell 
--------------------------------------------------------------------------------


-- | declare a Chip with name 'name' at _7_ bit bus address 'addr'
--
-- > $(chip "Chip123" 0x22)  =>
-- >   
-- > data Chip123
-- >
-- > instance Chip Chip123 where
-- >     chipAddress = 0x22
-- >     chipName = "Chip123"
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



-- | declare a register of Chip that contains Word8 data
--
-- > $(register1 "REG_SMALL" 0xF0 0x01) =>
-- > 
-- >   newtype REG_SMALL = REG_SMALL Word8
-- >
-- >   instance Register REG_SMALL where
-- >       registerAddress = 0xF0
-- >       registerName = "REG_SMALL"
-- >       regread = regreadRegister1
-- >       regwrite = regwriteRegister1
-- >       regmodify = regmodifyRegister1
-- >
-- >   instance Default REG_SMALL where
-- >       def = REG_SMALL 0x01
-- >
-- >   instance Show REG_SMALL where
-- >       show = showRegister1
-- >
register1 :: String -> RegisterAddress -> Word8 -> Q [Dec]
register1 name addr def = do
    let name' = mkName name
    dNewtype <- decNewtype name' ''Word8 []
    dInstanceRegister <- decInstanceRegister name' addr
    dInstanceDefault <- decInstanceDefault name' def
    dInstanceShow <- decInstanceShow name' 
    pure [dNewtype, dInstanceRegister, dInstanceDefault, dInstanceShow]
    
    where

      decInstanceRegister :: Name -> RegisterAddress -> Q Dec
      decInstanceRegister tname addr = do
          let dAddress = funD 'registerAddress $ one $ clause [] (normalB $ litE $ integerL $ fromRegisterAddress addr) []
              dName = funD 'registerName $ one $ clause [] (normalB $ litE $ stringL $ nameBase tname ) []
              dRead = decFunctionAlias 'regread 'regreadRegister1
              dWrite = decFunctionAlias 'regwrite 'regwriteRegister1
              dModify = decFunctionAlias 'regmodify 'regmodifyRegister1
          instanceD (cxt []) (appT (conT ''Register) (conT tname)) [dAddress, dName, dRead, dWrite, dModify]

      decInstanceShow :: Name -> Q Dec
      decInstanceShow tname =
          instanceD (cxt []) (appT (conT ''Show) (conT tname)) $ one $ decFunctionAlias 'Text.Show.show 'showRegister1



-- | declare a register of Chip that contains Word16
--
-- > $(register1 "REG_LARGE" 0xF0 0x0123) =>
-- > 
-- >   newtype REG_LARGE = REG_LARGE Word8
-- >       deriving (Show)
-- >   instance Register2 REG_LARGE where
-- >       registerAddress = 0xF0
-- >       registerName = "REG_LARGE"
-- >       regread = regread2'
-- >       regwrite = regwrite2'
-- >   instance Default REG_LARGE where
-- >       def = 0x0123
-- >   instance Show REG_LARGE where
-- >       show = showRegister2
-- 
register2 :: String -> RegisterAddress -> Word16 -> Q [Dec]
register2 name addr def = do
    let name' = mkName name
    dNewtype <- decNewtype name' ''Word16 []
    dInstanceRegister <- decInstanceRegister name' addr
    dInstanceDefault <- decInstanceDefault name' def
    dInstanceShow <- decInstanceShow name' 
    pure [dNewtype, dInstanceRegister, dInstanceDefault, dInstanceShow]
    
    where

      decInstanceRegister :: Name -> RegisterAddress -> Q Dec
      decInstanceRegister tname addr = do
          let dAddress = funD 'registerAddress $ one $ clause [] (normalB $ litE $ integerL $ fromRegisterAddress addr) []
              dName = funD 'registerName $ one $ clause [] (normalB $ litE $ stringL $ nameBase tname ) []
              dRead = decFunctionAlias 'regread 'regreadRegister2
              dWrite = decFunctionAlias 'regwrite 'regwriteRegister2
              dModify = decFunctionAlias 'regmodify 'regmodifyRegister2
          instanceD (cxt []) (appT (conT ''Register) (conT tname)) [dAddress, dName, dRead, dWrite, dModify]
    
      decInstanceShow :: Name -> Q Dec
      decInstanceShow tname =
          instanceD (cxt []) (appT (conT ''Show) (conT tname)) $ one $ decFunctionAlias 'Text.Show.show 'showRegister2


-- | declare a (sub)field of a register 

 --
 --   field ''MY_REG "MY_FIELD" "00000000000****0"
 -- ======>
 --   getMY_FIELD :: (Integral w1, Bits w1) => MY_REG -> w1
 --   getMY_FIELD = \(MY_REG w0) -> fromIntegral $ (unsafeShiftR w0 1 .&. 0b0000000000001111)
 --   setMY_FIELD :: (Integral w, Bits w) => w -> MY_REG -> MY_REG
 --   setMY_FIELD = \w1 (MY_REG w0) -> MY_REG $ ((w0 .&. complement 0b0000000000011110) .|. unsafeShiftL (0b0000000000001111 .&. fromIntegral w1) 1)
field :: Name -> String -> String -> Q [Dec]
field ty name bitstr = case bitstrToField bitstr of
    Left err              -> fail err
    Right sil@(size, ix, len) -> do
        tycon <- nameTyCon ty
        dGet <- decGet ty tycon sil
        dSet <- decSet ty tycon sil
        dBit <- if len == 1 then decBit ty sil else mempty
        pure $ dGet <> dSet <> dBit
    where
        nameTyCon :: Name -> Q Name
        nameTyCon ty = do
            TyConI (NewtypeD _ _ty _ _ (NormalC tycon [(_, ConT _tywrapped)]) _)  <- reify ty
            pure tycon

        -- ix == 3, len == 2 =>
        -- > getX :: (Integral w, Bits w) => REG -> w
        -- > getX = \(REG w) -> fromIntegral $ (shiftR w 3) .&. 0b11
        decGet ty tycon (size, ix, len) = do
            let funname = mkFunctionName $ "get" <> name  
                maskE = LitE $ IntegerL $ mkMaskN len 
            w0 <- newName "w0" 
            w1 <- newName "w1"
            pure  [ SigD funname (ForallT [] [AppT (ConT ''Integral) (VarT w1), AppT (ConT ''Bits) (VarT w1)] (AppT (AppT ArrowT (ConT ty)) (VarT w1)))
                  , ValD (VarP funname) (NormalB (LamE [ConP tycon [] [VarP w0]] 
                         (InfixE (Just (VarE 'fromIntegral)) (VarE '($)) (Just (InfixE (Just (AppE (AppE (VarE 'unsafeShiftR) (VarE w0)) (LitE (IntegerL $ fromIntegral ix)))) (VarE '(.&.)) (Just maskE)))))) []
                  ]

        decSet ty tycon (size, ix, len) = do
            let funname = mkFunctionName $ "set" <> name  
                maskE0 = LitE $ IntegerL $ mkMaskIxLen ix len 
                maskE1 = LitE $ IntegerL $ mkMaskN len 
                ixE    = LitE $ IntegerL $ fromIntegral ix
            w <- newName "w" 
            w0 <- newName "w0" 
            w1 <- newName "w1"
            pure  [ SigD funname (ForallT [] [AppT (ConT ''Integral) (VarT w), AppT (ConT ''Bits) (VarT w)] (AppT (AppT ArrowT (VarT w)) (AppT ( AppT ArrowT (ConT ty)) (ConT ty))))
                  , ValD (VarP funname) (NormalB (LamE [VarP w1,ConP tycon [] [VarP w0]] (InfixE (Just (ConE tycon)) (VarE '($)) (Just (InfixE (Just (InfixE (Just (VarE w0)) (VarE '(.&.)) (Just (AppE (VarE 'complement) (maskE0))))) (VarE '(.|.)) (Just (AppE (AppE (VarE 'unsafeShiftL) (InfixE (Just maskE1) (VarE '(.&.)) (Just (AppE (VarE 'fromIntegral) (VarE w1))))) ixE))))))) []]
            
        decBit ty (size, ix, len) = pure []

-- | 3 => 0b0111
mkMaskN :: Word -> Integer
mkMaskN n = 
    shiftL 0b1 (fromIntegral n) - 1

mkMaskIxLen :: Word -> Word -> Integer
mkMaskIxLen ix len = 
    shiftL (shiftL 0b1 (fromIntegral len) - 1) (fromIntegral ix)

mkFunctionName :: String -> Name
mkFunctionName str = 
    Name (OccName str) NameS

--------------------------------------------------------------------------------
--  helpers for Register instancing

regreadRegister1 :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, MonadIO m) => 
                    BusDevice chip -> m reg
regreadRegister1 busdev = 
    fmap coerce $ regread1 @chip busdev (registerAddress @reg)

regwriteRegister1 :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, Default reg, MonadIO m) => 
                     BusDevice chip -> (reg -> reg) -> m reg
regwriteRegister1 busdev = \f -> do
    let r' = f def
    regwrite1 @chip busdev (registerAddress @reg) (coerce $ r')
    pure r'

regmodifyRegister1 :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, MonadIO m) => 
            BusDevice chip -> (reg -> reg) -> m reg
regmodifyRegister1 busdev = \f -> do
    r <- regreadRegister1 busdev
    let r' = f r
    regwrite1 busdev (registerAddress @reg) $ coerce $ r'
    pure r'

     
regreadRegister2 :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, MonadIO m) => 
            BusDevice chip -> m reg
regreadRegister2 busdev =
    fmap coerce $ regread2 @chip busdev (registerAddress @reg)

regwriteRegister2 :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, Default reg, MonadIO m) => 
            BusDevice chip -> (reg -> reg) -> m reg
regwriteRegister2 busdev = \f -> do
    let r' = f def
    regwrite2 @chip busdev (registerAddress @reg) $ coerce $ r'
    pure r'

regmodifyRegister2 :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, MonadIO m) => 
            BusDevice chip -> (reg -> reg) -> m reg
regmodifyRegister2 busdev = \f -> do
    r <- regreadRegister2 busdev
    let r' = f r
    regwrite2 busdev (registerAddress @reg) $ coerce $ r'
    pure r'



--------------------------------------------------------------------------------
--  helpers

decInstanceDefault :: Integral value => Name -> value -> Q Dec
decInstanceDefault tname v = do
    let dDef :: Q Dec
        dDef = funD 'def $ one $ clause [] (normalB $ appE (conE tname) (litE $ integerL $ toInteger v)) []
    instanceD (cxt []) (appT (conT ''Default) (conT tname)) [dDef]


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

decFunctionAlias :: Name -> Name -> Q Dec
decFunctionAlias a b = 
    funD a $ one $ clause [] (normalB $ varE b) []

showRegister1 :: forall reg . (Register reg, Coercible Word8 reg) => reg -> String
showRegister1 reg = 
    (toString $ registerName @reg) <> "(" <> showWord8 (coerce reg) <> ")"
    --(toString $ registerName @reg) <> "==" <> showWord8 (coerce reg) 

showRegister2 :: forall reg . (Register reg, Coercible Word16 reg) => reg -> String
showRegister2 reg = 
    (toString $ registerName @reg) <> "(" <> showWord16 (coerce reg) <> ")"
    --(toString $ registerName @reg) <> "==" <> showWord16 (coerce reg) 


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
--  

showWord8 :: Word8 -> String
showWord8 = showWordN 2

showWord16 :: Word16 -> String
showWord16 = showWordN 4 

showWordN :: Integral a => Int -> a -> String
showWordN n w =
    let str = fmap toUpper $ showHex w ""
        n'  = if length str <= n then n - length str else 0
    in replicate n' '0' <> str
