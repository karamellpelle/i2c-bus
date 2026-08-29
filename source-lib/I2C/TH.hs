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


--------------------------------------------------------------------------------
--  Register

regread' :: forall chip reg m . (Chip chip, Register reg, Storable reg, MonadIO m) => BusDevice chip -> m reg
regread' busdev = 
    liftIO $ read busdev $ registerAddress @reg

regwrite' :: forall chip reg m . (Chip chip, Register reg, Storable reg, MonadIO m) => BusDevice chip -> reg -> m ()
regwrite' busdev = \r -> 
    liftIO $ write busdev $ StorableAB (registerAddress @reg) r


-- | declare a register of Chip from Storable type. Storable is 
--   relative to I2C chip
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
register8 :: String -> RegisterAddress -> Word8 -> Q [Dec]
register8 name addr def = do
    let name' = mkName name
    dReg <- registerN ''Store8 name addr
    dInstanceDefault <- decInstanceDefault name' def
    dInstanceShow <- decInstanceShow name' 'showBin8
    pure $ dReg <> [dInstanceDefault, dInstanceShow]

-- | declare a register of Chip that contains Word16 data as Little Endian
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
--   field ''REG16 "MY_FIELD" "00000000000****0"
-- ======>
--   getMY_FIELD :: (Integral w1) => MY_REG -> w1
--   getMY_FIELD = \(REG16 w0) -> fromIntegral $ (unsafeShiftR w0 1 .&. 0b0000000000001111)
--   setMY_FIELD :: (Integral w) => w -> REG16 -> REG16
--   setMY_FIELD = \w1 (REG16 w0) -> REG16 $ ((w0 .&. complement 0b0000000000011110) .|. unsafeShiftL (0b0000000000001111 .&. fromIntegral w1) 1)
--
--   field ''REG8 "MY_BIT" "00*00000"
-- ======>
--   getMY_BIT :: Integral w1 => REG8 -> w1
--   getMY_BIT = \(REG8 w0) -> (fromIntegral $ (unsafeShiftR w0 5 .&. 1))
--   setMY_BIT :: Integral w => w -> REG8 -> REG8
--   setMY_BIT = \w1 (REG8 w0) -> (REG8 $ ((w0 .&. complement 0b00100000) .|. unsafeShiftL (0b00000001 .&. fromIntegral w1) 5))
--   bitsetMY_BIT :: REG8 -> REG8
--   bitsetMY_BIT = under @Word8 (flip setBit 5)
--   bitclearMY_BIT :: REG8 -> REG8
--   bitclearMY_BIT = under @Word8 (flip clearBit 5)
--   bittoggleMY_BIT :: REG8 -> REG8
--   bittoggleMY_BIT = under @Word8 (flip complementBit 5)
field :: Name -> String -> String -> Q [Dec]
field ty name bitstr = case bitstrToField bitstr of
    Left err              -> fail err
    Right sil@(size, ix, len) -> do
        --info <- reify ty
        --runIO $ print info
  
        TyConI (NewtypeD _ _ty _ _ (NormalC tycon [(_, ConT tywrap)]) _)  <- reify ty
        dGet <- decGet tywrap ty tycon sil
        dSet <- decSet tywrap ty tycon sil
        dBit <- if len == 1 then decBit tywrap ty sil else mempty
        pure $ dGet <> dSet <> dBit
    where

        --getTEST_F :: Integral n => MY_REG16 -> n ; getTEST_F = \w -> fromIntegral $ unsafeShiftR (un @Store16LE w) 13 .&. 1

        decGet tywrap ty tycon (size, ix, len) = do
            let funname = mkFunctionName $ "get" <> name  
                maskE = LitE $ IntegerL $ mkMaskN len 
            n <- newName "n" 
            w <- newName "w" 
            --w1 <- newName "w1"
            --pure  [ SigD funname (ForallT [] [AppT (ConT ''Integral) (VarT w1)] (AppT (AppT ArrowT (ConT ty)) (VarT w1)))
            --      , ValD (VarP funname) (NormalB (LamE [ConP tycon [] [VarP w0]] 
            --             (InfixE (Just (VarE 'fromIntegral)) (VarE '($)) (Just (InfixE (Just (AppE (AppE (VarE 'unsafeShiftR) (VarE w0)) (LitE (IntegerL $ fromIntegral ix)))) (VarE '(.&.)) (Just maskE)))))) []
            --      ]

            pure  [ SigD funname (ForallT [] [AppT (ConT ''Integral) (VarT n)] (AppT (AppT ArrowT (ConT ty)) (VarT n)))
                  , ValD (VarP funname) (NormalB (LamE [VarP w] (InfixE (Just (VarE 'fromIntegral)) (VarE '($)) (Just (InfixE (Just (AppE (AppE (VarE 'unsafeShiftR) 
                  (AppE (AppTypeE (VarE 'un) (ConT tywrap)) (VarE w))) (LitE (IntegerL $ fromIntegral ix)))) (VarE '(.&.)) (Just maskE)))))) []
                  ]

                  --[ SigD getTEST_F_2 (ForallT [] [AppT (ConT GHC.Internal.Real.Integral) (VarT n_1)] (AppT (AppT ArrowT (ConT GHCI.MY_REG16)) (VarT n_1)))
                  --, ValD (VarP getTEST_F_2) (NormalB (LamE [VarP w_3] (InfixE (Just (VarE GHC.Internal.Real.fromIntegral)) (VarE GHC.Internal.Base.$) (Just (InfixE (Just (AppE (AppE (VarE GHC.Internal.Bits.unsafeShiftR) (AppE (AppTypeE (VarE Relude.Extra.Newtype.un) (ConT I2C.Types.Store16LE)) (VarE w_3))) (LitE (IntegerL 13)))) (VarE GHC.Internal.Bits..&.) (Just (LitE (IntegerL 1)))))))) []]

        decSet tywrap ty tycon (size, ix, len) = do
            let funname = mkFunctionName $ "set" <> name  
                maskE0 = LitE $ IntegerL $ mkMaskIxLen ix len 
                maskE1 = LitE $ IntegerL $ mkMaskN len 
                ixE    = LitE $ IntegerL $ fromIntegral ix
            w <- newName "w" 
            w0 <- newName "w0" 
            w1 <- newName "w1"
            pure  [ SigD funname (ForallT [] [AppT (ConT ''Integral) (VarT w)] (AppT (AppT ArrowT (VarT w)) (AppT ( AppT ArrowT (ConT ty)) (ConT ty))))
                  , ValD (VarP funname) (NormalB (LamE [VarP w1,ConP tycon [] [VarP w0]] (InfixE (Just (ConE tycon)) (VarE '($)) (Just (InfixE (Just (InfixE (Just (VarE w0)) (VarE '(.&.)) (Just (AppE (VarE 'complement) (maskE0))))) (VarE '(.|.)) (Just (AppE (AppE (VarE 'unsafeShiftL) (InfixE (Just maskE1) (VarE '(.&.)) (Just (AppE (VarE 'fromIntegral) (VarE w1))))) ixE))))))) []]
            
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
mkFunctionName str = 
    Name (OccName str) NameS

--fieldGet :: (Integral w1, Bits w1) => Word -> w0 -> (w0 -> w1)
--fieldGet ix mask = \w0 -> fromIntegral $ (unsafeShiftR w0 (fromIntegral ix) .&. mask)
--
--    getMY_FIELD :: (Integral w1, Bits w1) => MY_REG -> w1
--    getMY_FIELD = \(MY_REG w0) -> fromIntegral $ (unsafeShiftR w0 1 .&. 0b0000000000001111)
--    setMY_FIELD :: (Integral w, Bits w) => w -> MY_REG -> MY_REG
--    setMY_FIELD = \w1 (MY_REG w0) -> MY_REG $ ((w0 .&. complement 0b0000000000011110) .|. unsafeShiftL (0b0000000000001111 .&. fromIntegral w1) 1)
--



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

