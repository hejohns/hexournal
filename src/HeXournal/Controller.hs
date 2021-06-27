-- HeXournal/Controller.hs
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

module HeXournal.Controller
  ( UIState
  ) where

import HeXournal.Model

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Cairo

import GHC.Int
import Data.IORef

type pxCoordinate = Coordinate
data UIState = UIState
  { model :: Model
  , pxPerMm :: Double
  , xy00 :: pxCoordinate
  , xyMax :: pxCoordinate
  , scratchStroke :: ()
  }

initUI :: IO Gtk.Application
initUI = do
  app <- new Gtk.Application [#applicationId := "hexournal"]
  _ <- on app #activate (do
      appMainWin <- new Gtk.ApplicationWindow [#title := "appMainWin", #application := app]
      drawingArea <- new Gtk.DrawingArea [#name := "drawingArea", #hasTooltip := True]
      set appMainWin [#child := drawingArea]
      --
      uiState <- newIORef newUIState
      --
      Gtk.drawingAreaSetDrawFunc drawingArea $ Just $ cbOnDrawingAreaDraw uiState
      _ <- after drawingArea #resize $ cbOnDrawingAreaResize uiState
      gestureStylus <- new Gtk.GestureStylus [#name := "gestureStylus"]
      _ <- on gestureStylus #down $ cbOnStylusDown uiState
      _ <- on gestureStylus #motion $ cbOnStylusMotion uiState
      _ <- on gestureStylus #proximity $ cbOnStylusProximity uiState
      _ <- on gestureStylus #up $ cbOnStylusUp uiState
      --
      id #show appMainWin
      )
  _ <- #run app Nothing
  return app
  where
  newUIState :: UIState
  newUIState = UIState
    { model = newModel doc
    , pxPerMm = undefined
    , xy00 = (0, 0)
    , xyMax = undefined
    , scratchStroke = ()
    }

cbOnDrawingAreaDraw :: UIState -> (Gtk.DrawingArea -> GI.Cairo.Context -> Int32 -> Int32 -> IO ())
cbOnDrawingAreaDraw uiState dA cr w h = withManagedPtr cr $ \cr -> draw_cb cr undefined
cbOnDrawingAreaResize :: UIState -> (Int32 -> Int32 -> IO ())
cbOnDrawingAreaResize uiState w h = modifyIORef' uiState $ \x -> x {xyMax = (w, h)}
cbOnStylusDown :: UIState -> (Double -> Double -> IO ())
cbOnStylusMotion :: UIState -> (Double -> Double -> IO ())
cbOnStylusProximity :: UIState -> (Double -> Double -> IO ())
cbOnStylusUp :: UIState -> (Double -> Double -> IO ())
