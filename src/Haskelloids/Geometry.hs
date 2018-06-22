-- Geometry.hs, A module for translating, rotating and scaling 2D shapes and simple-polygons (i.e. non-self-crossing).

-- NB: IMPORTANT - All the operations concerning intersections of polygons assume all polygons are convex and are supplied with their vertices in clockwise order

module Haskelloids.Geometry (Shape(..), Figure(..), origin, add, inv, len, dist,
                             shape, intersect, Graphic, Point, Point2, Angle,
                             polyline, ellipse, regularPolygon, inCircle, inPolygon
                            ) where

import Graphics.HGL.Units (Point)
import Graphics.HGL.Draw.Monad (Graphic)
import Graphics.HGL.Draw.Picture (polyline, ellipse)

import Data.Ratio ((%))
import Data.List (nub)

import Numeric.LinearAlgebra (Matrix, Vector, (|>), (#>), (<>), det, ident, fromLists, toList)

type Angle = Double
type Point2 = (Double, Double)
type Segment = (Point2, Point2)

-- #### Datatype defintions ####################################################

-- Shape - a drawable shape, either an unbroken polygon, a circle, or a line
data Shape = Poly [Point2]
           | Circ Point2 Double
           | Ln [Point2]
 deriving (Show)

-- Figure - purely functional representation of affine transformations of shapes and lines
data Figure = Polygon [Point2]
            | Circle Point2 Double
            | Line [Point2]
            | Translate Point2 Figure
            | Scale Double Figure
            | Rotate Angle Figure
 deriving (Show)

-- #### Program constants ######################################################

origin :: Point2
origin = (0, 0)

-- #### Function definitions ###################################################

add :: Point2 -> Point2 -> Point2
add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

sub :: Point2 -> Point2 -> Point2
sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

inv :: Point2 -> Point2
inv (x,y) = (-x,-y)

len :: Point2 -> Double
len (x, y) = sqrt $ x^2 + y^2

dist :: Point2 -> Point2 -> Double
dist (x,y)(u,v) = len (u-x,v-y)

dot :: Point2 -> Point2 -> Double
dot (x1,y1) (x2,y2) = (x1*x2 + y1*y2)

rot :: Point2 -> Angle -> Point2
rot (x,y) theta =
  let x' = cos theta * x - sin theta * y
      y' = sin theta * x + cos theta * y
  in (x', y')

segments :: [Point2] -> [Segment]
segments ps = zipWith (,) ps (tail . cycle $ ps)

shape :: Figure -> Shape
shape f = shape' (ident 3) f
 where
   -- shape' - uses homogeneous coordinates so we can do affine translations (i.e. linearly dependant transformations)
   shape' :: Matrix Double -> Figure -> Shape
   shape' m (Polygon ps)         = Poly . map (transform m) $ ps'
     where ps' = makeCycle ps
   shape' m (Line ps)            = Ln . map (transform m) $ ps
   shape' m (Circle c r)         = Circ (transform m c) ((r *) . sqrt . det $ m) -- The radius changes by the sqrt of the determinant of the transformation
   shape' m (Translate (x, y) f) = shape' (m <> m') f
     where m' = fromLists [[1, 0, x],
                           [0, 1, y],
                           [0, 0, 1]]
   shape' m (Scale s f)          = shape' (m <> m') f
     where m' = fromLists [[s, 0, 0],
                           [0, s, 0],
                           [0, 0, 1]]
   shape' m (Rotate phi f)       = shape' (m <> m') f
     where m' = fromLists [[cos phi,-sin phi, 0],
                           [sin phi, cos phi, 0],
                           [0,       0,       1]]
   makeCycle :: [Point2] -> [Point2]
   makeCycle (p:ps) = if p == last ps
                       then (p:ps)
                       else (p:ps) ++ [p]
   toHomo :: Point2 -> Vector Double
   toHomo (x, y) = (3 |> [x, y, 1])
   transform :: Matrix Double -> Point2 -> Point2
   transform m p = (x, y)
     where (x:y:_) = (toList . (m #>) . toHomo) p

regularPolygon :: Int -> Double -> Figure
regularPolygon n r | n < 3     = error "regularPolygon - cannot construct a regular polygon of less than 3 sides"
                   | otherwise = Polygon $ take (n+1) [(r * cos t, r * sin t) | t <- [0, (2*pi)/(fromIntegral n)..]]

intersect :: Shape -> Shape -> Bool
intersect (Ln _)     _          = False
intersect _          (Ln _)     = False
intersect (Circ c r) (Circ d s) = abs (dist c d) <= r + s
intersect (Poly ps)  (Circ c r) = inPolygon ps c || any (inCircle c r) ps
intersect (Circ c r) (Poly ps)  = inPolygon ps c || any (inCircle c r) ps
intersect (Poly ps)  (Poly qs)  = any (inPolygon qs) ps
                                   || any (inPolygon ps) qs

inPolygon :: [Point2] -> Point2 -> Bool
inPolygon ps p@(x,y) = odd . length . filter intersectRay . segments $ ps
  where
   intersectRay :: Segment -> Bool
   intersectRay s@(p1@(x1,y1),p2@(x2,y2)) = (p `xBetween` p1 $ p2)
                                             && x * grad s + yintrsct s >= y
   xBetween :: Point2 -> Point2 -> Point2 -> Bool
   xBetween (x1,_) (x2,_) (x3,_) = (x2 <= x1 && x1 <= x3)
                                   || (x3 <= x1 && x1 <= x2)

inCircle :: Point2 -> Double -> Point2 -> Bool
inCircle (xc,yc) r (x,y) = r^2 >= ((xc-x)^2 + (yc-y)^2)

yintrsct :: Segment -> Double
yintrsct ((x,y),(u,v)) = y - ((x * (v - y)) / (u - x))

grad :: Segment -> Double
grad ((x,y),(u,v)) = (v - y) / (u - x)
