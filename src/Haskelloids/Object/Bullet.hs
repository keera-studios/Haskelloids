{-# LANGUAGE Arrows #-}
module Haskelloids.Object.Bullet (bulletSF
                                 ) where

-- import control structures
import Control.Arrow (returnA)

-- import Yampa framework
import FRP.Yampa (Event(..), integral, edge, time, (^<<), (<<<), arr, merge)

-- import graphics and geometry
import Haskelloids.Geometry (Point, Point2, Angle, Figure(..), origin, shape)
import Haskelloids.Graphics (Graphic, drawShape)

-- import game objects and interface
import Haskelloids.Object (Object, ObjectClass(..), ObjectInput(..),
                           ObjectOutput(..), teleport)

-- #### Program constants ######################################################

-- bulletFigure - the bullet figure
bulletFigure :: Figure
bulletFigure = Circle origin 2

-- buffer - window edge buffer size, stops the object appearing when teleporting from one edge to the other
buffer :: Int
buffer = 2

-- bulletSpeed - speed bullet travels in pixels per second
bulletSpeed :: Double
bulletSpeed = 500

-- bulletMaxAge - the maximum number of seconds a bullet will exist for
bulletMaxAge :: Double
bulletMaxAge = 0.75

-- #### Signal function ########################################################

bulletSF :: Point -> Point2 -> Point2 -> Angle -> Object
bulletSF (w, h) (x0, y0) (vx0, vy0) o =
  let !vx = vx0 + bulletSpeed * cos o
      !vy = vy0 + bulletSpeed * sin o
  in proc oi -> do
       x <- teleport w buffer x0 -< vx
       y <- teleport h buffer y0 -< vy

       let bulShape = shape . Translate (x,y) $ bulletFigure

       die <- edge <<< (> bulletMaxAge) ^<< time -< ()
       hit <- arr oiHit -< oi

       returnA -<
          ObjectOutput {
            ooPos      = (x,y),
            ooCllsnBox = bulShape,
            ooGraphic  = drawShape bulShape,
            ooSpawnReq = NoEvent,
            ooObjClass = Bullet,
            ooKillReq  = merge die hit
          }

-- #### Function definitions ###################################################
