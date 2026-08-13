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
{-# LANGUAGE CPP #-}
module I2C.TH
(

  chip,
  register1,
  register2,

  regread1',
  regwrite1',
  regmodify1',
  regread2',
  regwrite2',
  regmodify2',
) where

import Relude hiding (Type)
import I2C
import Data.Default
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

#ifdef I2C_INTERNAL_LINUX
import I2C.Internal.Linux qualified as Internal
#endif
#ifdef I2C_INTERNAL_EMPTY
import I2C.Internal.Empty qualified as Internal
#endif

-- | declare a Chip type 'name' at _7_ bit bus address 'reg'
--
-- > $(chip "Chip123" 0x22) -- =>
-- >   
-- > data Chip123
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



-- | declare a register of Chip containing Word8
--
-- > $(register1 "REG_SMALL" 0xF0 0x01) =>
-- > 
-- >   newtype REG_SMALL = REG_SMALL Word8
-- >       deriving (Show)
-- >   instance Register REG_SMALL where
-- >       registerAddress = 0xF0
-- >       registerName = "REG_SMALL"
-- >       regread = regread1'
-- >       regwrite = regwrite1'
-- >       regmodify = regmodify1'
-- >   instance Default REG_SMALL where
-- >       def = REG_SMALL 0x01
-- >
-- >

register1 :: String -> RegisterAddress -> Word8 -> Q [Dec]
register1 name addr def = do
    let name' = mkName name
    dNewtype <- decNewtype name' ''Word8 [''Show]
    dInstanceRegister <- decInstanceRegister name' addr
    dInstanceDefault <- decInstanceDefault name' def
    pure [dNewtype, dInstanceRegister, dInstanceDefault]
    
    where

      decInstanceRegister :: Name -> RegisterAddress -> Q Dec
      decInstanceRegister tname addr = do
          let dAddress = funD 'registerAddress $ one $ clause [] (normalB $ litE $ integerL $ fromRegisterAddress addr) []
              dName = funD 'registerName $ one $ clause [] (normalB $ litE $ stringL $ nameBase tname ) []
              dRead = funD 'regread $ one $ clause [] (normalB $ varE 'regread1') []
              dWrite = funD 'regwrite $ one $ clause [] (normalB $ varE 'regwrite1') []
              dModify = funD 'regmodify $ one $ clause [] (normalB $ varE 'regmodify1') []
          instanceD (cxt []) (appT (conT ''Register) (conT tname)) [dAddress, dName, dRead, dWrite, dModify]

decInstanceDefault :: Integral value => Name -> value -> Q Dec
decInstanceDefault tname v = do
    let dDef :: Q Dec
        dDef = funD 'def $ one $ clause [] (normalB $ appE (conE tname) (litE $ integerL $ toInteger v)) []
    instanceD (cxt []) (appT (conT ''Default) (conT tname)) [dDef]


-- | declare a register of Chip containing Word8
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
-- >
-- >
register2 :: String -> RegisterAddress -> Word16 -> Q [Dec]
register2 name addr def = do
    let name' = mkName name
    dNewtype <- decNewtype name' ''Word16 [''Show]
    dInstanceRegister <- decInstanceRegister name' addr
    dInstanceDefault <- decInstanceDefault name' def
    pure [dNewtype, dInstanceRegister, dInstanceDefault]
    
    where

      decInstanceRegister :: Name -> RegisterAddress -> Q Dec
      decInstanceRegister tname addr = do
          let dAddress = funD 'registerAddress $ one $ clause [] (normalB $ litE $ integerL $ fromRegisterAddress addr) []
              dName = funD 'registerName $ one $ clause [] (normalB $ litE $ stringL $ nameBase tname ) []
              dRead = funD 'regread $ one $ clause [] (normalB $ varE 'regread2') []
              dWrite = funD 'regwrite $ one $ clause [] (normalB $ varE 'regwrite2') []
              dModify = funD 'regmodify $ one $ clause [] (normalB $ varE 'regmodify2') []
          instanceD (cxt []) (appT (conT ''Register) (conT tname)) [dAddress, dName, dRead, dWrite, dModify]



{-
FIXME: create TH functions for the following constructs


$(field   ''SYSTEM1 "XTAL_DIV" "0***0000") =>

  -- setter and getter. field size 3 at bit index 5
  setXTAL_DIV :: Word8 -> SYSTEM1 -> SYSTEM1
  getXTAL_DIV :: SYSTEM1 -> Word8

$(field   ''SYSTEM1 "TX_ENABLE" "000*0000") =>
    
  -- setter and getter
  setTX_ENABLE :: Word8 -> TX_ENABLE -> TX_ENABLE
  getTX_ENABLE :: TX_ENABLE -> Word8
  -- bit utils, since field has size only 1 
  setbitTX_ENABLE    :: TX_ENABLE -> TX_ENABLE
  clearbitTX_ENABLE  :: TX_ENABLE -> TX_ENABLE
  togglebitTX_ENABLE :: TX_ENABLE -> TX_ENABLE
  getbitTX_ENABLE    :: TX_ENABLE -> Bool


-- | bitstring to begin index and field length
bitsToField Word8 -> (Word8, Word8) 
bitsToField bitstr = strip 0 bitstr
    where
        strip ix as = case as of
            ('*':as') -> count ix 0 as
            (_:as')   -> strip (succ ix) as'
            []        -> rest ix as -- FIXME
        count ix len as = case as of
            ('*':as') -> count ix (succ len) as' -- TODO: assert ix + len <= 8
            _         -> rest ix len (ix + len) as'
        rest ix len total as -> case as of
            []        -> -- TODO: assert total <= 8, return
            ('*':_)   -> fail "field is not connected"
            (_:as')   -> rest ix len (succ total) as'
-}

--------------------------------------------------------------------------------
--  helpers

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

              
regread1' :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, MonadIO m) => 
            Internal.BusDevice chip -> m reg
regread1' busdev = 
    fmap coerce $ regread1 @chip busdev (registerAddress @reg)

regwrite1' :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, Default reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regwrite1' busdev = \f -> do
    let r' = f def
    regwrite1 @chip busdev (registerAddress @reg) (coerce $ r')
    pure r'

regmodify1' :: forall chip reg m . (Chip chip, Register reg, Coercible Word8 reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regmodify1' busdev = \f -> do
    r <- regread1' busdev
    let r' = f r
    regwrite1 busdev (registerAddress @reg) $ coerce $ r'
    pure r'

     
regread2' :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, MonadIO m) => 
            Internal.BusDevice chip -> m reg
regread2' busdev =
    fmap coerce $ regread2 @chip busdev (registerAddress @reg)

regwrite2' :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, Default reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regwrite2' busdev = \f -> do
    let r' = f def
    regwrite2 @chip busdev (registerAddress @reg) $ coerce $ r'
    pure r'

regmodify2' :: forall chip reg m . (Chip chip, Register reg, Coercible Word16 reg, MonadIO m) => 
            Internal.BusDevice chip -> (reg -> reg) -> m reg
regmodify2' busdev = \f -> do
    r <- regread2' busdev
    let r' = f r
    regwrite2 busdev (registerAddress @reg) $ coerce $ r'
    pure r'

-- TODO: regxxxN

