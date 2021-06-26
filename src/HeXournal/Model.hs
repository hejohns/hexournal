-- HeXournal/Model.hs
--
-- * The 'M' in MVC
-- *
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

module HeXournal.Model
  ( Model
  , Document
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

--            Undo Stack  Current   Redo Stack
type Model = ([Document], Document, [Document])
--               2, 1    3     4, 5
type Document = ([Page], Page, [Page])
data Page = Page
  { xyMax :: Coordinate
  , background00 :: Coordinate
  , background :: Background
  --           2, 1     3      4, 5
  , layers :: ([Layer], Layer, [Layer])
  }

data Background
  = Blank
  | LinedA4
  | LinedUS

type Layer = [Stroke]
data Stroke
  = Pen {color :: Color, width :: Double, xy :: [Coordinate]}
  | PressurePen {color :: Color, wxy :: [(Double, Coordinate)]}
  | Highlighter {color :: Color, width :: Double, xy :: [Coordinate]}

type Color = Int
type Coordinate = (Double, Double)

newModel :: Document -> Model
newModel doc = ([], doc, [])

newDocument :: Coordinate -> Coordinate -> Background -> Document
newDocument wh b00 b = ([], [newPage wh b00 b], [])

newDefaultA4Document :: Document
newDefaultA4Document = newDocument (210, 297) (0, 0) LinedA4

newDefaultUSDocument :: Document
newDefaultUSDocument = newDocument (216, 279) (0, 0) LinedUS

newPage :: Coordinate -> Coordinate -> Background -> Page
newPage wh b00 b =
  Page {xyMax = wh, background00 = b00, background = b, layers = ([], [], [])}
