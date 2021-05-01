{-# LANGUAGE GADTs #-}

module HeXournal.Core ( Document
                      , Stroke(..)
) where

import Numeric
import Data.List

type Document = [Page]
data Page where Page :: Background -> [Layer] -> Page
data Background where
  Lined :: Background
type Layer = [Stroke]
data Stroke where
  Pen :: Color -> Width -> [Coordinate] -> Stroke
  Highlighter :: Color -> Width -> [Coordinate] -> Stroke
type Color = Integer
type Width = Double
data Coordinate where Coordinate :: Double -> Double -> Coordinate

newDocument :: Maybe Background -> Document
newDocument Nothing = [Page Lined []]
newDocument (Just b) = [Page b []]

erase :: Layer -> Coordinate -> Width -> Layer
erase [] _ _ = []
erase strokes (Coordinate x y) w = map id strokes

erase' :: Stroke -> Coordinate -> Width -> Stroke
erase' s@(Pen _ _ []) _ _ = s
erase' s@(Highlighter _ _ []) _ _ = s
erase' s@(Pen _ _ coordinates) coord w = s

removeEmptyStrokes :: Layer -> Layer
removeEmptyStrokes = filter (\s ->
    case s of
    Pen _ _ coordinates -> not $ null coordinates
    Highlighter _ _ coordinates -> not $ null coordinates)
