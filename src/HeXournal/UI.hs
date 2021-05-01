{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module HeXournal.UI (
    initUI
) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Cairo
import qualified GI.Cairo.Render as Cairo
import GI.Cairo.Render.Connector (renderWithContext)
--import qualified GI.Gdk as Gdk
import GHC.Int
import Data.IORef

initUI :: IO Gtk.Application
initUI = do
  ioRef <- newIORef ""
  app <- new Gtk.Application [#applicationId := "hexournal"]
  on app #activate (activate app ioRef)
  id #run app Nothing
  return app

activate :: Gtk.Application -> IORef a -> IO ()
activate app ioRef = do
  win <- new Gtk.ApplicationWindow [#application := app, #title := "title"]
  dA <- new Gtk.DrawingArea []
  set win [#child := dA]
  return False
  id #setDrawFunc dA (Just (drawCb ioRef))
  after dA #resize resizeCb
  id #show win
  return ()

drawCb :: IORef a -> Gtk.DrawingArea -> GI.Cairo.Context -> Int32 -> Int32 -> IO ()
drawCb ioRef drawingArea cr width height = do
{-
  renderWithContext (Cairo.setSourceRGB (fromIntegral 0) (fromIntegral 0) 1)  cr
  renderWithContext Cairo.fill cr
  renderWithContext Cairo.paint cr
  renderWithContext (Cairo.showText ("a" :: [Char])) cr
  putStrLn "drawing"
  -}
  renderWithContext (Cairo.setSourceRGB 1 0 1) cr
  renderWithContext (Cairo.moveTo 0 0) cr
  renderWithContext (Cairo.lineTo 100 100) cr
  renderWithContext (Cairo.moveTo 100 0) cr
  renderWithContext (Cairo.lineTo 0 100) cr
  renderWithContext (Cairo.setLineWidth 20) cr
  renderWithContext (Cairo.fill) cr
  renderWithContext (Cairo.rectangle 0 0 50 50) cr
  renderWithContext Cairo.stroke cr
  renderWithContext (Cairo.setSourceRGB 0 0 1) cr
  renderWithContext (Cairo.setLineWidth 1) cr
  renderWithContext (Cairo.moveTo 200 200) cr
  renderWithContext (Cairo.relLineTo 100 100) cr
  renderWithContext (Cairo.relLineTo 0 200) cr
  renderWithContext (Cairo.closePath) cr
  renderWithContext (Cairo.strokePreserve) cr
  --renderWithContext (Cairo.fill) cr
  --renderWithContext (Cairo.mask Cairo.patt
  --get cr #target
  --Cairo.setSourceSurface cr 

resizeCb _ _ = do
  putStrLn "resized"
