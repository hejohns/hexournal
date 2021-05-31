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

module HeXournal.Model
  ( Document
  , Page(Page)
  , Background(..)
  , Layer
  , Stroke(..)
  , Color
  , Coordinate
  --
  , newDocument
  , newDefaultA4Document
  , newDefaultUSDocument
  , newPage
  ) where

type Document = [Page]
data Page = Page
  { width :: Double
  , height :: Double
  , background :: Background
  , layers :: [Layer]
  }

data Background
  = Blank
  | Lined

type Layer = [Stroke]
data Stroke
  = Pen {color :: Color, width :: Double, xy :: [Coordinate]}
  | PressurePen {color :: Color, wxy :: [(Double, Coordinate)]}
  | Highlighter {color :: Color, width :: Double, xy :: [Coordinate]}

type Color = Int
data Coordinate = Coordinate
  { x :: Double
  , y :: Double
  }

newDocument :: (Double, Double) -> Background -> Document
newDocument wh b = [newPage wh b]

newDefaultA4Document :: Document
newDefaultA4Document = newDocument (210, 297) Lined

newDefaultUSDocument :: Document
newDefaultUSDocument = newDocument (216, 279) Lined

newPage :: (Double, Double) -> Background -> Page
newPage (w, h) b = 
  Page {width = w, height = h, background = b, layers = []}
