{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}

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
import Control.Monad.Reader

data UIState = UIState {drawingArea :: IORef Gtk.DrawingArea, document :: IORef Document, scratchStroke :: IORef Stroke}

initUI :: IO Gtk.Application
initUI = do
  app <- new Gtk.Application [#applicationId := "hexournal"]
  _ <- on app #activate (do
      dA <- newIORef undefined
      doc <- newIORef $ newDocument $ Just Lined
      ss <- newIORef (Pen 0 0 [])
      let uiState = UIState { drawingArea = dA
                            , document = doc
                            , scratchStroke = ss
                            }
      --
      appWin <- new Gtk.ApplicationWindow [#application := app, #startupId := "appWin id", #title := "appWin title"]
      drawingArea <- new Gtk.DrawingArea [#hasTooltip := True, #name := "drawingArea"]
      set appWin [#child := drawingArea]
      -- slightly irked out by that overload
      --id #setDrawFunc drawingArea (Just (drawCb ioRef))
      Gtk.drawingAreaSetDrawFunc drawingArea $ Just (drawCb uiState)
      _ <- after drawingArea #resize resizeCb
      gestureStylus <- new Gtk.GestureStylus [#name := "gestureStylus"]
      Gtk.widgetAddController drawingArea gestureStylus
      _ <- on gestureStylus #down $ stylusDownCb uiState
      _ <- on gestureStylus #motion $ stylusMotionCb uiState
      _ <- on gestureStylus #proximity $ stylusProximityCb uiState
      _ <- on gestureStylus #up $ stylusUpCb uiState
      id #show appWin
      )
  _ <- #run app Nothing
  return app

drawCb :: UIState -> (Gtk.DrawingArea -> GI.Cairo.Context -> Int32 -> Int32 -> IO ())
drawCb uiState drawingArea' cr width height = do
  writeIORef (drawingArea uiState) drawingArea'
{-
  renderWithContext (Cairo.setSourceRGB (fromIntegral 0) (fromIntegral 0) 1)  cr
  renderWithContext Cairo.fill cr
  renderWithContext Cairo.paint cr
  renderWithContext (Cairo.showText ("a" :: [Char])) cr
  putStrLn "drawing"
  -}
  -- when record dot syntax gets here
  --document <- readIORef uiState.document
  doc <- readIORef $ document uiState
  ss <- readIORef $ scratchStroke uiState
  runReaderT (drawDocument doc) cr
  runReaderT (drawStroke ss) cr
  --renderWithContext (Cairo.setSourceRGB 1 0 1) cr
  --renderWithContext (Cairo.moveTo 0 0) cr
  --renderWithContext (Cairo.lineTo 100 100) cr
  --renderWithContext (Cairo.moveTo 100 0) cr
  --renderWithContext (Cairo.lineTo 0 100) cr
  --renderWithContext (Cairo.setLineWidth 20) cr
  --renderWithContext (Cairo.fill) cr
  --renderWithContext (Cairo.rectangle 0 0 50 50) cr
  --renderWithContext Cairo.stroke cr
  --renderWithContext (Cairo.setSourceRGB 0 0 1) cr
  --renderWithContext (Cairo.setLineWidth 1) cr
  --renderWithContext (Cairo.moveTo 200 200) cr
  --renderWithContext (Cairo.relLineTo 100 100) cr
  --renderWithContext (Cairo.relLineTo 0 200) cr
  --renderWithContext (Cairo.closePath) cr
  --renderWithContext (Cairo.strokePreserve) cr
  --renderWithContext (Cairo.fill) cr
  --renderWithContext (Cairo.mask Cairo.patt
  --get cr #target
  --Cairo.setSourceSurface cr 
  return ()

drawDocument :: Document -> ReaderT GI.Cairo.Context IO ()
drawDocument = mapM_ drawPage

drawPage :: Page -> ReaderT GI.Cairo.Context IO ()
drawPage (Page background layers) = do
  cr <- ask
  drawBackground background
  mapM_ drawLayer layers

drawBackground :: Background -> ReaderT GI.Cairo.Context IO ()
drawBackground Blank = do
  cr <- ask
  liftIO $ renderWithContext (Cairo.setSourceRGB 0 0 0) cr
  liftIO $ renderWithContext Cairo.fill cr
drawBackground Lined = do
  --for testing
  cr <- ask
  liftIO $ renderWithContext (Cairo.setSourceRGB 1 0 0) cr
  liftIO $ renderWithContext (Cairo.moveTo 100 0) cr
  liftIO $ renderWithContext (Cairo.lineTo 100 1000) cr
  liftIO $ renderWithContext Cairo.stroke cr

drawLayer :: Layer -> ReaderT GI.Cairo.Context IO ()
drawLayer = mapM_ drawStroke

drawStroke :: Stroke -> ReaderT GI.Cairo.Context IO ()
drawStroke (Pen color width coordinates) = do
  cr <- ask
  liftIO $ renderWithContext (Cairo.setSourceRGB 0 0 (fromIntegral color)) cr
  liftIO $ renderWithContext (Cairo.setLineWidth width) cr
  drawStroke' coordinates
drawStroke (Highlighter color width coordinates) = do
  cr <- ask
  liftIO $ renderWithContext (Cairo.setSourceRGBA 0 0 (fromIntegral color) 0.5) cr
  liftIO $ renderWithContext (Cairo.setLineWidth width) cr
  drawStroke' coordinates

drawStroke' :: [Coordinate] -> ReaderT GI.Cairo.Context IO ()
drawStroke' coords = do
  cr <- ask
  case coords of
    hd1:hd2:tl -> do
      liftIO $ renderWithContext (uncurry Cairo.moveTo hd1) cr
      liftIO $ renderWithContext (uncurry Cairo.lineTo hd2) cr
      liftIO $ renderWithContext Cairo.stroke cr
      drawStroke' (hd2:tl)
    _ -> liftIO $ return ()

resizeCb :: Int32 -> Int32 -> IO ()
resizeCb _ _ = do
  putStrLn "resized"

stylusDownCb :: UIState -> (Double -> Double -> IO())
stylusDownCb uiState x y = do
  let ss = scratchStroke uiState
  writeIORef ss (Pen 1 0.5 [(x, y)])

stylusMotionCb :: UIState -> (Double -> Double-> IO())
stylusMotionCb uiState x y = do
  let dA = drawingArea uiState
  dA' <- readIORef dA
  let ss = scratchStroke uiState
  modifyIORef' ss (\case {
      Pen color width coords -> Pen color width (coords ++ [(x, y)])
     ;Highlighter color width coords -> Highlighter color width (coords ++ [(x, y)])
     })
  Gtk.widgetQueueDraw  dA'

stylusProximityCb :: UIState -> (Double -> Double -> IO())
stylusProximityCb uiState x y = do
  return ()

stylusUpCb :: UIState -> (Double -> Double-> IO())
stylusUpCb uiState x y = do
  let dA = drawingArea uiState
  dA' <- readIORef dA
  let doc = document uiState
  let ss = scratchStroke uiState
  ss' <- readIORef ss
  modifyIORef' doc (`addStroke` ss')
  Gtk.widgetQueueDraw  dA'

addStroke :: Document -> Stroke -> Document
addStroke (dhd:dtl) stroke = case dhd of
  Page background (lhd:ltl) -> (Page background ((lhd ++ [stroke]):ltl)):dtl
  Page background [] -> (Page background ([[stroke]])):dtl
addStroke [] stroke = undefined
