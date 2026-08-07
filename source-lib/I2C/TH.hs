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
  --register1,

) where

import Relude hiding (Type)
import I2C.Class
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

-- | declare a Chip type 'name' at _7_ bit bus address 'reg'
--
-- > $(chip "BT4050" 0x22) -- =>
-- >   
-- > data BT4050
-- > instance Chip BT4050 where
-- >     chipAddress = 0x22
-- >     chipName = "BT4050"
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



-- | declare a Register of Chip 
--
-- > $(register1 ''Chip123 "REG_A" 0xF0 0x01) =>
-- > 
-- >   newtype REG_A = REG_A Word8
-- >       deriving (Show)
-- >   instance Register Chip123 REG_A where
-- >       registerAddress = 0xF0
-- >       registerName = "REG_A"
-- >   instance Default REG_A where
-- >       def = 0x01
-- >
-- >

{-
register1 :: Name -> String -> RegisterAddress -> Word8 -> Q [Dec]
register1 chip name addr def = do
    let name' = mkName name
    dNewtype <- decNewtype name' ''Word8 [''Show]
    dInstanceRegister <- decInstanceRegister chip name' addr
    --dDefault  <- decInstanceDefault name' def
    --pure [dNewtype, dInstanceRegister]
    pure [dNewtype, dInstanceRegister]
    
    where

      decInstanceRegister :: Name -> Name -> RegisterAddress -> Q Dec
      decInstanceRegister cname tname addr = do
          let dAddress :: Q Dec
              dAddress = funD 'registerAddress $ one $ clause [] (normalB $ litE $ integerL $ fromRegisterAddress addr) []
              dName :: Q Dec
              dName = funD 'registerName $ one $ clause [] (normalB $ litE $ stringL $ nameBase tname ) []
          instanceD (cxt []) (appT (conT ''Register) (conT tname)) [dAddress, dName]


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
-}

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
