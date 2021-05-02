{-# LANGUAGE GADTs #-}
-- we don't actually use GADTs, but imo it looks nicer

module HeXournal.Core ( Document
                      , Page(Page)
                      , Background(Blank, Lined)
                      , Layer
                      , Stroke(..)
                      , Color
                      , Width
                      , Coordinate
                      , newDocument
) where

import Numeric
import Data.List

type Document = [Page]
data Page where Page :: Background -> [Layer] -> Page
data Background where
  Blank :: Background
  Lined :: Background
type Layer = [Stroke]
data Stroke where
  Pen :: Color -> Width -> [Coordinate] -> Stroke
  Highlighter :: Color -> Width -> [Coordinate] -> Stroke
type Color = Integer
type Width = Double
type Coordinate = (Double, Double)

newDocument :: Maybe Background -> Document
newDocument Nothing = [Page Lined []]
newDocument (Just b) = [Page b []]

erase :: Layer -> Coordinate -> Width -> Layer
erase [] _ _ = []
erase strokes (x, y) w = map id strokes

erase' :: Stroke -> Coordinate -> Width -> Stroke
erase' s@(Pen _ _ []) _ _ = s
erase' s@(Highlighter _ _ []) _ _ = s
erase' s@(Pen _ _ coordinates) coord w = s

removeEmptyStrokes :: Layer -> Layer
removeEmptyStrokes = filter (\s ->
    case s of
    Pen _ _ coordinates -> not $ null coordinates
    Highlighter _ _ coordinates -> not $ null coordinates)
