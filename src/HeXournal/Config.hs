-- HeXournal/Config.hs
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-- 
-- +Or see <http://www.gnu.org/licenses/>.
-- +Additional information on the GPL(v2) can be found there.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- for TypeError

module HeXournal.Config
  ( HeXournalConfig
  , defaultConfig
  ) where

import GHC.TypeLits

data Config :: (Symbol, Symbol) -> *

type family FirstSymOK c where
  FirstSymOK (Config "one") = True
  FirstSymOK (Config "two") = True
  FirstSymOK x = TypeError (Text "Not a valid config")

class FirstSymOK c ~ True => FirstSym c where
  doTheThing :: c -> String

instance FirstSym (Config ("a", b)) where
  doTheThing _ = "blah"

type family SecondSymOK c where
  SecondSymOK (Config "three") = True
  SecondSymOK (Config "four") = True
  SecondSymOK x = TypeError (Text "Not a valid config")

class SecondSymOK c ~ True => SecondSym c where
  doTheOtherThing :: c -> Int

defaultConfig :: ("a", "b")
defaultConfig = undefined
