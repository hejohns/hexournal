{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

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
import HeXournal.Core

initUI :: IO Gtk.Application
initUI = do
  app <- new Gtk.Application [#applicationId := "hexournal"]
  _ <- on app #activate (do
      document <- newIORef ([] :: Document)
      scratchStroke <- newIORef (Pen 0 0 [])
      foo <- newIORef undefined
      --
      appWin <- new Gtk.ApplicationWindow [#application := app, #startupId := "appWin id", #title := "appWin title"]
      drawingArea <- new Gtk.DrawingArea [#hasTooltip := True, #name := "drawingArea"]
      set appWin [#child := drawingArea]
      -- slightly irked out by that overload
      --id #setDrawFunc drawingArea (Just (drawCb ioRef))
      Gtk.drawingAreaSetDrawFunc drawingArea (Just (drawCb foo))
      _ <- after drawingArea #resize resizeCb
      gestureStylus <- new Gtk.GestureStylus [#name := "gestureStylus"]
      Gtk.widgetAddController drawingArea gestureStylus
      _ <- on gestureStylus #down $ stylusDownCb undefined
      _ <- on gestureStylus #proximity $ stylusProximityCb undefined
      _ <- on gestureStylus #motion $ stylusMotionCb undefined
      _ <- on gestureStylus #up $ stylusUpCb undefined
      id #show appWin
      )
  _ <- #run app Nothing
  return app

drawCb :: IORef GI.Cairo.Context -> (Gtk.DrawingArea -> GI.Cairo.Context -> Int32 -> Int32 -> IO ())
drawCb ioRef drawingArea cr width height = do
{-
  renderWithContext (Cairo.setSourceRGB (fromIntegral 0) (fromIntegral 0) 1)  cr
  renderWithContext Cairo.fill cr
  renderWithContext Cairo.paint cr
  renderWithContext (Cairo.showText ("a" :: [Char])) cr
  putStrLn "drawing"
  -}
  writeIORef ioRef cr
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

stylusDownCb :: IORef GI.Cairo.Context -> Double -> Double-> IO()
stylusDownCb ioRef x y = do
  cr <- readIORef ioRef
  renderWithContext (Cairo.setSourceRGB 0 1 0) cr
  renderWithContext (Cairo.fill) cr

stylusProximityCb :: IORef GI.Cairo.Context -> Double -> Double-> IO()
stylusProximityCb ioRef x y = do
  cr <- readIORef ioRef
  renderWithContext (Cairo.setSourceRGB 0 1 1) cr
  renderWithContext (Cairo.fill) cr

stylusMotionCb :: IORef GI.Cairo.Context -> Double -> Double-> IO()
stylusMotionCb ioRef x y = do
  cr <- readIORef ioRef
  renderWithContext (Cairo.setSourceRGB 1 1 0) cr
  renderWithContext (Cairo.fill) cr

stylusUpCb :: IORef GI.Cairo.Context -> Double -> Double-> IO()
stylusUpCb ioRef x y = do
  cr <- readIORef ioRef
  renderWithContext (Cairo.setSourceRGB 0 1 1) cr
  renderWithContext (Cairo.fill) cr
