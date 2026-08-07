-- Copyright (C) 2026 karamellpelle@hotmail.com
-- 
-- This file is part of 'piradio'.
-- 
-- 'piradio' is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- 'piradio' is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with 'piradio'.  If not, see <http://www.gnu.org/licenses/>.
--
{-# LANGUAGE TemplateHaskell #-}
module Main
(
    main,
) where

import Relude
import Text.Pretty.Simple
import I2C

$(chip "Chip123" 0x20)

main :: IO ()
main = do
   
    putTextLn $ "chipName:    " <> (chipName @Chip123)
    putTextLn $ "chipAddress: " <> (show $ chipAddress @Chip123)

    busdev <- openChip @Chip123 "/dev/null"
    pPrint busdev

    pure ()
