{-# LANGUAGE Arrows #-}
module Haskelloids.Object.Asteroid (RoidSz(..),
                                    asteroidSF
                                   )where

import Control.Arrow (returnA, arr)
import Control.Monad (replicateM, liftM)

import Control.Monad.Random (RandomGen, Rand, evalRand, getRandomR)
import Control.Monad.Random.Class (getSplit)

import FRP.Yampa (Event(..), next, (<<^), isEvent, edge, tag)

import Haskelloids.Object (Object, ObjectClass(..), ObjectInput(..),
                           ObjectOutput(..), teleport)
import Haskelloids.Object.Dust (makeDust)

import Haskelloids.Geometry (Point, Point2, Angle, Figure(..), shape)
import Haskelloids.Graphics (drawShape)

-- #### Datatype definitions ###################################################

data RoidSz = RdSmall
            | RdMedium
            | RdLarge
 deriving (Eq, Enum)

-- #### Program constants ######################################################

roidFigure1, roidFigure2, roidFigure3, roidFigure4 :: Figure

roidFigure1 = Polygon [(-32,-15), (-9,-15), (-16,-30), (8,-30), (31,-15),
                       (32,-7), (8,0), (31,15), (16,30), (7,23), (-16,30),
                       (-32,8), (-32,-15)]

roidFigure2 = Polygon [(-17,-31), (0,-24), (16,-30), (31,-16), (17,-8), (31,7),
                       (15,29), (-8,22), (-17,29), (-32,14), (-25,-1),
                       (-32,-16), (-17,-31)]

roidFigure3 = Polygon [(-10,-30), (16,-30), (31,-8), (31,7), (16,30), (0,30),
                       (0,8), (-16,29), (-32,7), (-17,0), (-32,-8), (-10,-30)]

roidFigure4 = Polygon [(-16,-29), (1,-18), (18,-29), (35,-15), (23,2), (34,17),
                       (9, 33), (-16,33), (-32,17), (-32,-16), (-16,-29)]

roidCllsnFigure1, roidCllsnFigure2, roidCllsnFigure3, roidCllsnFigure4 :: Figure

roidCllsnFigure1 = Polygon [(-32,-15), (-32,8.0), (-16,30), (7,23), (16,30),
                            (31, 15), (32, -7), (31, -15), (8, -30), (-16, -30),
                            (-32,-15)]

roidCllsnFigure2 = Polygon [(-32, -16), (-32, 14), (-17, 29), (15, 29), (31, 7),
                            (17, -8), (31, -16), (16, -30), (-17, -31),
                            (-32, -16)]

roidCllsnFigure3 = Polygon [(-32, -8), (-32, 7), (-16, 29), (0, 30), (16, 30),
                            (31, 7), (31, -8), (16, -30), (-10, -30), (-32, -8)]

roidCllsnFigure4 = Polygon [(-32, -16), (-32, 17), (-16, 33), (9, 33), (34, 17),
                            (23, 2), (35, -15), (18, -29), (-16, -29),
                            (-32, -16)]

buffer :: Int
buffer = 40

minSpeed, maxSpeed :: Double
minSpeed = 30
maxSpeed = 100

-- #### Signal functions #######################################################

-- asteroidSF - the signal function for an asteroid object NB: COULD ABSTRACT ALL THIS TO MONAD-RANDOM?
asteroidSF :: RandomGen g => g -> Point -> Point2 -> Double -> Angle -> RoidSz -> Double -> Object
asteroidSF g (w, h) (x0, y0) s o sz fig =
  let s'   = minSpeed + ((maxSpeed - minSpeed) * s)
      !vx  = s' * cos o
      !vy  = s' * sin o
      !buf = round $ fromIntegral buffer * scale sz
      (fg',cllsnFig) = case fig of
                        r | r <= 0.25 -> (roidFigure1, roidCllsnFigure1)
                          | r <= 0.5  -> (roidFigure2, roidCllsnFigure2)
                          | r <= 0.75 -> (roidFigure3, roidCllsnFigure3)
                          | otherwise -> (roidFigure4, roidCllsnFigure4)

  in proc oi -> do
       x <- teleport w buf x0 -< vx
       y <- teleport h buf y0 -< vy

       let roidShape = shape . Translate (x,y) . Scale (scale sz) $ fg'

       hit <- arr oiHit -< oi

       returnA -< ObjectOutput {
                    ooPos      = (x,y),
                    ooCllsnBox = roidShape,
                    ooGraphic  = drawShape roidShape,
                    ooSpawnReq = hit `tag` (flip evalRand g $ do {
                                               frag <- replicateM 2 . makeFragment (w,h) (x,y) s $ sz
                                             ; dust <- replicateM 8 . makeDust $ (x,y)
                                             ; return (dust ++ if sz /= RdSmall
                                                                then frag
                                                                else [])
                                             }),
                    ooObjClass = Asteroid,
                    ooKillReq  = hit
                  }

-- #### Function defintions ####################################################

scale :: RoidSz -> Double
scale RdLarge  = 1
scale RdMedium = 0.5
scale RdSmall  = 0.25

makeFragment :: RandomGen g => Point -> Point2 -> Double -> RoidSz -> Rand g Object
makeFragment (w, h) (x, y) s sz = do
  g   <- getSplit
  x0  <- (x+) `liftM` getRandomR (-10.0,10.0)
  y0  <- (y+) `liftM` getRandomR (-10.0,10.0)
  s'  <- (s*) `liftM` getRandomR (0.7,1.3)
  o'  <- getRandomR (0, 2*pi)
  fig <- getRandomR (0, 1)
  return (asteroidSF g (w,h) (x0,y0) s' o' (pred sz) fig)
