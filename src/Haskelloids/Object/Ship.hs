{-# LANGUAGE Arrows #-}
module Haskelloids.Object.Ship (shipSF,
                                shipFigure
                               ) where

import Control.Arrow (returnA, (***))

import Haskelloids.Geometry (Figure(..), Shape(..), Point, Point2, Angle, shape)
import Haskelloids.Graphics (Graphic, drawFigure, drawShape)

import FRP.Yampa (Event(..), (^<<), (<<^), (<<<), arr, constant, hold, integral,
                  gate, isEvent, tag, mergeBy)

import Haskelloids.Input (UserInput(..))
import Haskelloids.Object (Object, ObjectClass(..), ObjectInput(..),
                           ObjectOutput(..), teleport, reload)
import Haskelloids.Object.Bullet (bulletSF)

-- #### Program constants ######################################################

-- shipFigure - the shape of the ship relative to the origin, facing 0 radians (East)
shipFigure :: Figure
shipFigure = Polygon [(15, 0), (-15, 10), (-9, 8), (-9, -8), (-15, -10), (15, 0)]

thrustersFig :: Figure
thrustersFig = Polygon [(-9, 8), (-16, 0), (-9, -8)]

-- cllsnFigure - the shape of the collidable area
cllsnFigure :: Figure
cllsnFigure = Polygon [(15, 0), (-15, 10), (-15, -10), (15, 0)]

-- buffer - window edge buffer size, stops the ship appearing when teleporting from one edge to the other
buffer :: Int
buffer = 30

-- bullet - the point where the bullets come out
bulletBox :: Double
bulletBox = 15

-- turnRate - radians per second
turnRate :: Double
turnRate = 4

-- accel - acceleration in pixels per second per second
accel :: Double
accel = 350

-- frictionLoss - proportion of velocity lost due to friction
frictionLoss :: Double
frictionLoss = 0.6

-- reloadTime - delay ship takes to reload between user fire events
reloadTime :: Double
reloadTime = 0.1

-- thrusterFlickerPeriod - the period, in seconds of the thruster's flicker
thrusterFlickerPeriod :: Double
thrusterFlickerPeriod = 0.05

-- #### Signal functions #######################################################

-- shipSF - given window dimensions and an initial position, create a space-ship signal function, the ship will teleport back to the opposite side of the screen when it passes off any of the game window edges.
shipSF :: Point -> Point2 -> Object
shipSF (w, h) (x0, y0) = proc oi -> do
  let ui = oiUserInput oi
  l  <- (\d -> if d then -turnRate else 0.0) ^<< hold False -< uiTurnLeft  ui
  r  <- (\d -> if d then  turnRate else 0.0) ^<< hold False -< uiTurnRight ui

  -- calculate orientation...
  o  <- ((-pi/2)+) ^<< integral -< l + r

  -- ...velocity and acceleration...
  t  <- hold False -< uiThrust ui
  th <- arr (\d -> if d then accel else 0.0) -< t
  let tx = th * cos o
      ty = th * sin o

  rec
    ax <- uncurry (-) ^<< (returnA *** ((* frictionLoss) ^<< integral)) -< (tx, ax)
    ay <- uncurry (-) ^<< (returnA *** ((* frictionLoss) ^<< integral)) -< (ty, ay)

  vx <- integral -< ax
  vy <- integral -< ay

  -- ...position
  x  <- teleport w buffer x0 -< vx
  y  <- teleport h buffer y0 -< vy

  -- is the user firing? have we reloaded our guns?
  f  <- reload reloadTime -< uiFire ui

  -- are we drawing the thrusters?
  dt <- reload thrusterFlickerPeriod <<^ gate (Event ()) -< t

  -- check for crash
  die <- arr oiHit -< oi

  -- ...return observable state
  returnA -<
    ObjectOutput {
      ooPos      = (x,y),
      ooCllsnBox = shape . Translate (x,y) . Rotate o $ cllsnFigure,
      ooGraphic  = do { drawFigure . Translate (x,y) . Rotate o $ shipFigure
                      ; if isEvent dt
                         then drawFigure . Translate (x,y) . Rotate o $ thrustersFig
                         else return () },
      ooSpawnReq = f `tag` [blltSpwn (x,y) (vx,vy) o],
      ooObjClass = Ship,
      ooKillReq  = die
    }
 where
   -- blltSpwn - create a new bullet signal function
   blltSpwn :: Point2 -> Point2 -> Angle -> Object
   blltSpwn (x0,y0) (vx, vy) o =
     let (x, y) = (x0 + (bulletBox * cos o),
                   y0 + (bulletBox * sin o))
     in bulletSF (w, h) (x,y) (vx, vy) o

-- #### Function definitions ###################################################
