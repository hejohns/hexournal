-- HeXournal/Model.hs
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
-- +Additional information on the GPL(v2) (and unofficial translations)
-- +can be found there.

module HeXournal.Model ( Document
                       , Page(Page)
                       , Background(..)
                       , Layer
                       , Stroke(..)
                       , Color
                       , Coordinate
                       --
                       , newDocument
                       ) where

data Document = Document [Page] Page [Page]
data Page = Page Background [Layer]
data Background = Blank
                | Lined
type Layer = [Stroke]
data Stroke = Pen {color :: Color, width :: Double, xy :: [Coordinate]}
            | PressurePen {color :: Color, wxy :: [(Double, Coordinate)]}
            | Highlighter {color :: Color, width :: Double, xy :: [Coordinate]}
type Color = Int
newtype Coordinate = Coordinate {x :: Double, y :: Double}

newDocument :: Background -> Document
newDocument b = Document [] (b []) []
