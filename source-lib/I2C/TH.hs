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

) where

import Relude hiding (Type)
import I2C.Class
import Data.Default
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

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
-- >   instance Register1 REG_SMALL where
-- >       registerAddress = 0xF0
-- >       registerName = "REG_SMALL"
-- >   instance Default REG_SMALL where
-- >       def = REG_SMALL 0x01
-- >
-- >

register1 :: String -> RegisterAddress -> Word8 -> Q [Dec]
register1 name addr def = do
    let name' = mkName name
    dNewtype <- decNewtype name' ''Word8 [''Show]
    dInstanceRegister <- decInstanceRegister1 name' addr
    dInstanceDefault <- decInstanceDefault name' def
    pure [dNewtype, dInstanceRegister, dInstanceDefault]
    
    where

      decInstanceRegister1 :: Name -> RegisterAddress -> Q Dec
      decInstanceRegister1 tname addr = do
          let dAddress :: Q Dec
              dAddress = funD 'register1Address $ one $ clause [] (normalB $ litE $ integerL $ fromRegisterAddress addr) []
              dName :: Q Dec
              dName = funD 'register1Name $ one $ clause [] (normalB $ litE $ stringL $ nameBase tname ) []
          instanceD (cxt []) (appT (conT ''Register1) (conT tname)) [dAddress, dName]

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
-- >   instance Default REG_LARGE where
-- >       def = 0x0123
-- >
-- >
register2 :: String -> RegisterAddress -> Word16 -> Q [Dec]
register2 name addr def = do
    let name' = mkName name
    dNewtype <- decNewtype name' ''Word16 [''Show]
    dInstanceRegister <- decInstanceRegister2 name' addr
    dInstanceDefault <- decInstanceDefault name' def
    pure [dNewtype, dInstanceRegister, dInstanceDefault]
    
    where

      decInstanceRegister2 :: Name -> RegisterAddress -> Q Dec
      decInstanceRegister2 tname addr = do
          let dAddress :: Q Dec
              dAddress = funD 'register2Address $ one $ clause [] (normalB $ litE $ integerL $ fromRegisterAddress addr) []
              dName :: Q Dec
              dName = funD 'register2Name $ one $ clause [] (normalB $ litE $ stringL $ nameBase tname ) []
          instanceD (cxt []) (appT (conT ''Register2) (conT tname)) [dAddress, dName]


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
