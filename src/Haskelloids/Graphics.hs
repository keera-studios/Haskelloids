module Haskelloids.Graphics (Graphic,
                             drawShape,
                             drawFigure
                            ) where

import Control.Arrow ((***))

import Graphics.HGL.Draw (Graphic)
import Graphics.HGL.Draw.Picture (ellipse, polyline)

import Haskelloids.Geometry (Figure, Shape(..), shape)

drawFigure :: Figure -> Graphic
drawFigure f = drawShape . shape $ f

drawShape :: Shape -> Graphic
drawShape (Ln ps)        = polyline . map (round *** round) $ ps
drawShape (Poly ps)      = polyline . map (round *** round) $ ps
drawShape (Circ (x,y) r) = ellipse (x'-r', y'-r') (x'+r', y'+r')
  where x' = round x
        y' = round y
        r' = round r
