-- #### Haskelloids.hs

-- TODO Implement hyperspace jump
-- TODO Implement bonus scores
-- TODO Implement bonus life

{-# LANGUAGE Arrows, ScopedTypeVariables #-}
module Main (main) where

-- Import random
import System.Random (newStdGen)

-- Import control structures
import Control.Arrow (returnA, (&&&))
import Control.Monad (replicateM, liftM, liftM2)
import Control.Monad.Random (Rand, RandomGen(..), getRandomR, getRandomRs,
                             evalRand, evalRandIO, runRand)
import Control.Monad.Random.Class (getSplit)

-- Import data structures
import Data.IdentityList (IList, ILKey)
import qualified Data.IdentityList as IL (empty, elems, insert, delete,
                                          insertMany, assocs, fromList,
                                          mapWithKey)
import Data.Version (Version(..), showVersion)

-- Import geometry and graphics
import Graphics.HGL (runGraphics, overGraphics)
import Graphics.HGL.Window (Window, RedrawMode(..), openWindowEx, setGraphic,
                            getWindowTick)
import Graphics.HGL.Draw.Text (text)
import Haskelloids.Geometry (Figure(..), Point, Point2, inCircle, regularPolygon)
import Haskelloids.Graphics (Graphic, drawFigure)
import qualified Haskelloids.Geometry.Text as Text (Alignment(..), fromString)

-- Import Yampa framework
import FRP.Yampa (Event(..), SF, DTime, arr, constant, identity, (>>>), (<<<),
                  (>>^), (^>>), (^<<), (<<^), reactimate, dpSwitch, notYet,
                  accum, accumHold, tag, edge, switch, dSwitch, never, after,
                  delayEvent, mergeBy, lMerge, event, isEvent, tagWith, gate)

-- Import game input
import Haskelloids.Input (UserInput(..), getWindowEvs, procWindowEvs, inputInit)

-- import game objects
import Haskelloids.Object (Object, ObjectClass(..), ObjectInput(..),
                           ObjectOutput(..), hits)
import Haskelloids.Object.Ship (shipSF, shipFigure)
import Haskelloids.Object.Asteroid (RoidSz(..), asteroidSF)

-- Import path info
import qualified Paths_Haskelloids as Path (version)

-- #### Datatype definitions ###################################################

type Score = Int
type Lives = Int

data GameInput = GameInput {
                   giUserInput :: UserInput,
                   giNewRound  :: Event Int,
                   giRespawn   :: Event ()
                 }

data GameOutput = GameOutput {
                    goGraphic  :: Graphic,
                    goGameOver :: Event Score
                  }

-- #### Program constants ######################################################

-- verNo - version number
verNo :: String
verNo = showVersion $ Path.version { versionTags = [] }

-- ## Window constants ##########################

-- wWidth, wHeight - the heigh and width of the game window in Pixels
wWidth, wHeight :: Int
wWidth  = 800
wHeight = 600

-- wDim - window dimensions
wDim :: Point
wDim = (wWidth, wHeight)

-- refreshPeriod' - the refresh period of the graphics in milliseconds
refreshPeriod' :: Integer
refreshPeriod' = 15

-- refreshPeriod - the refresh period of the graphics in seconds
refreshPeriod :: Double
refreshPeriod = fromInteger refreshPeriod' * 10^^(-3)

-- ## Game constants ############################

-- centre - the center of the game window
centre :: Point2
centre = (centreX, centreY)

-- centerX, centerY - the center of the game window
centreX, centreY :: Double
centreX = fromIntegral $ wWidth`div`2
centreY = fromIntegral $ wHeight`div`2

-- safeRadius - the radius of the area from the central point that asteroids cannot spawn in
safeRadius :: Double
safeRadius = 75

-- initAsteroidsCount - the number of initial asteroids
initAsteroidsCount :: Int
initAsteroidsCount = 5

-- initLivesCount - initial number of lives in a game
initLivesCount :: Int
initLivesCount = 3

-- killPoints - points per asteroid kill
killPoints :: Int
killPoints = 100

-- respawnDelay - the delay between player and asteroid respawns
respawnDelay :: DTime
respawnDelay = 2

-- ## Graphics constants ########################

-- lifeFigure - the figure drawn for a life counter
lifeFigure :: Figure
lifeFigure = Rotate (-pi/2) shipFigure

-- #### Function definitions ###################################################

-- ## Main game loop ############################

-- init - called once at the start of program for setting up game environment
--        returns - the input to the game signal function at time zero
initUI :: IO UserInput
initUI = return inputInit

-- sense - captures any game inputs (e.g. user) and returns them along with the time since the last invocation of sense
--         NB: any processing that takes longer than the window tick will cause a frame to skip
--   w      - target window for rendering game objects
--   _      - action may block, we however won't be doing any blocking actions
--   return - #1 time (in seconds) since last invocation, #2 any game input
sense :: Window -> Bool -> IO (DTime, Maybe UserInput)
sense w _ = do
  evs <- getWindowEvs w
  let gi = procWindowEvs inputInit evs -- fold window events over game input
  getWindowTick w
  return (refreshPeriod, Just gi)

-- actuate - renders the collection of signal functions, conforming to the specification of Yampa's "reactimate"
--   w      - window to render to
--   _      - Has the output changed from the last sample, but we will assume it always has
--   out    - Current output sample
--   return - Termination flag
actuate :: Window -> Bool -> Graphic -> IO Bool
actuate w _ out = do
  setGraphic w out
  return False -- never terminate

-- initAsteroids - create initial asteroid objects
initAsteroids :: RandomGen g => Int -> Point2 -> Rand g [Object]
initAsteroids n safe = replicateM n (createAsteroid safe)

-- initGameObjects - all initial game objects
initGameObjects :: RandomGen g => Rand g (IList Object)
initGameObjects = do
  let ship = shipSF (wWidth, wHeight) centre
  rds <- initAsteroids initAsteroidsCount centre
  return (IL.fromList $ ship : rds)

-- route - routes incoming object input to the correct signal function
route :: forall sf. (GameInput, IList ObjectOutput)
                      -> IList sf
                      -> IList (ObjectInput, sf)
route (gi,oos) sfs = IL.mapWithKey routeAux $ sfs
  where
    routeAux :: ILKey -> sf -> (ObjectInput, sf)
    routeAux k obj = (ObjectInput { oiHit = if k `elem` hs
                                              then Event ()
                                              else NoEvent,
                                    oiUserInput = giUserInput gi },
                      obj)

    hs = hits . IL.assocs $ oos

-- killOrSpawn - adds and removes signal functions upon requests from the game objects, NB: we get passed the old [ObjectOuput] and the new [ObjectOutput]
killOrSpawn :: RandomGen g => g -> ((GameInput, IList ObjectOutput), IList ObjectOutput) -> Event (IList Object -> IList Object)
killOrSpawn g ((gi, _), oos) =
  let es = [ mergeBy (.)
               (ooKillReq oo `tag` flip IL.delete k)
               (fmap (flip IL.insertMany) (ooSpawnReq oo))
           | (k,oo) <- IL.assocs oos ]
      safeCentre = case filter ((== Ship) . ooObjClass) . IL.elems $ oos of
                     []  -> centre
                     [s] -> ooPos s
      respawn    = giRespawn gi `tag` flip IL.insert (shipSF wDim centre)
      newRound   = (flip IL.insertMany . flip evalRand g . flip initAsteroids safeCentre) `fmap` giNewRound gi
  in foldl (mergeBy (.)) NoEvent (respawn : newRound : es)

-- createAsteroid - create a randomly generated asteroid outside the "safe area" circle
createAsteroid :: RandomGen g => Point2 -> Rand g Object
createAsteroid (cx, cy) = do
  g <- getSplit
  xs <- getRandomRs (0, fromIntegral wWidth)
  ys <- getRandomRs (0, fromIntegral wHeight)
  let (x,y) = head . dropWhile unsafe . zip xs $ ys
  s <- getRandomR (0, 1)
  o <- getRandomR (0, 2*pi)
  f <- getRandomR (0, 1)

  return (asteroidSF g (wWidth, wHeight) (x,y) s o RdLarge f)
 where
  pair (x:y:xs) = (x,y) : pair xs
  unsafe (x,y)  = inCircle (cx,cy) safeRadius (x,y)

-- asteroidsKilled - returns a function that increments the score for each asteroid killed
asteroidsKilled :: IList ObjectOutput -> Event (Score -> Score)
asteroidsKilled oos = foldr (mergeBy (.)) NoEvent
                        . map (flip tag (+killPoints) . ooKillReq)
                        . filter ((== Asteroid) . ooObjClass) . IL.elems $ oos

drawObjectOutput :: IList ObjectOutput -> Graphic
drawObjectOutput = overGraphics . map ooGraphic . IL.elems

drawScore :: Score -> Graphic
drawScore = overGraphics . map (drawFigure . Translate (50,50))
              . Text.fromString Text.AlignLeft . show

drawLives :: Lives -> Graphic
drawLives = overGraphics . map drawFigure
              . zipWith Translate [(x,125) | x <- [60,83..]]
              . flip replicate lifeFigure

-- ## Signal functions ##########################

-- allAsteroidsKilled - returns an event after all the asteroids were destroyed
allAsteroidsKilled :: SF (IList ObjectOutput) (Event (Int -> Int))
allAsteroidsKilled =
  tagWith (+1) ^<< edge <<^ not . any ((== Asteroid) . ooObjClass) . IL.elems

-- playerCrashed - returns the an event the moment the player crashes, also returns the second event when it is safe to respawn
playerCrashed :: SF (IList ObjectOutput) (Event (), Event ())
playerCrashed = proc oos -> do
  crash <- crashed -< oos
  spawn <- respawn -< oos
  returnA -< (crash, spawn)
 where
  crashed :: SF (IList ObjectOutput) (Event ())
  crashed = any (\s -> (isEvent . ooKillReq $ s) && (ooObjClass s == Ship)) . IL.elems ^>> edge
  respawn :: SF (IList ObjectOutput) (Event ())
  respawn = switch (never &&& (crashed >>> delayEvent respawnDelay)) (const waitUntilSafe)
  waitUntilSafe :: SF (IList ObjectOutput) (Event ())
  waitUntilSafe = dSwitch (safe &&& safe) (const respawn)
  safe :: SF (IList ObjectOutput) (Event ())
  safe = arr (gate (Event ()) . not . any (inCircle centre safeRadius . ooPos) . filter ((== Asteroid) . ooObjClass) . IL.elems)

-- gameCore - the signal function that orchestrates the collection of visible signal function objects, such as removing collided objects and spawning new asteroids and ships
gameCore :: RandomGen g => g -> IList Object -> SF (GameInput, IList ObjectOutput) (IList ObjectOutput)
gameCore g objs =
  let (g',_) = split g
  in dpSwitch route objs (arr (killOrSpawn g) >>> notYet) (\sfs f -> gameCore g' (f sfs))

-- gameRound - the signal function that is responsible for playing a single "round" of asteroids, maintaining the player score and life counter
gameRound :: RandomGen g => g -> IList Object -> SF UserInput GameOutput
gameRound g objs = proc ui -> do
  rec
    (crashed,respawn) <- playerCrashed -< oos
    score <- accumHold 0 -< (crashed `tag` (+0)) `lMerge` (asteroidsKilled oos)
    lives <- accumHold initLivesCount -< mergeBy (.) (NoEvent `tag` (+1)) (respawn `tag` (subtract 1))
    newRound <- accum initAsteroidsCount <<< delayEvent respawnDelay <<< allAsteroidsKilled -< oos
    let gi = GameInput {
               giUserInput = ui,
               giNewRound  = newRound,
               giRespawn   = respawn
             }
    oos <- gameCore g objs -< (gi, oos)

  gameOver <- edge -< lives <= 0

  returnA -< GameOutput {
               goGraphic  = overGraphics [drawObjectOutput oos,
                                          drawScore score,
                                          drawLives lives      ],
               goGameOver = gameOver `tag` score
             }

-- menu - the signal function that is responsible for displaying menu events
menu :: RandomGen g => g -> SF UserInput Graphic
menu g = proc ui -> do
  rec
    let gi = GameInput {
               giUserInput = ui,
               giNewRound  = NoEvent,
               giRespawn   = NoEvent
             }
    oos <- gameCore g objs -< (gi, oos)
  returnA -< do { drawObjectOutput oos ; menuGraphics }
 where
  objs :: IList Object
  objs = IL.fromList . flip evalRand g . initAsteroids 8 $ (centreX + 100, centreY + 100)
  menuGraphics :: Graphic
  menuGraphics = do
    overGraphics . map (drawFigure . Translate (centreX, centreY))
      . Text.fromString Text.AlignCentre $ "haskelloids"
    overGraphics . map (drawFigure . Translate (centreX, centreY+100) . Scale 0.5)
      . Text.fromString Text.AlignCentre $ "click to start"

-- gameOver - the signal function that displays the score and waits for user click to go back to the menu
gameOver :: Score -> SF UserInput Graphic
gameOver scr = constant graphics
 where
  graphics :: Graphic
  graphics = do
    overGraphics . map (drawFigure . Translate (centreX, centreY))
      . Text.fromString Text.AlignCentre $ "final score " ++ show scr

-- playRound - play a single round of the game
playRound :: RandomGen g => g -> IList Object -> SF UserInput (Graphic, Event Score)
playRound g init = proc ui -> do
  go <- gameRound g init -<  ui
  returnA -< (goGraphic go, goGameOver go)

-- endlessLoop - play endless rounds
endlessLoop :: RandomGen g => g -> SF UserInput Graphic
endlessLoop g =
  let (initObjs, g') = flip runRand g initGameObjects
      (g'', _) = split g
  in switch (menu g &&& arr uiLeftClick)
       (\_ -> switch (playRound g' initObjs)
                (\sc -> switch (gameOver sc &&& arr uiLeftClick) (\_ -> endlessLoop g'')))

-- ## Main function #############################

main :: IO ()
main = do
  g <- newStdGen
  runGraphics $ do
    w <- openWindowEx ("Haskelloids v" ++ verNo) Nothing (wWidth, wHeight)
           DoubleBuffered (Just refreshPeriod')
    reactimate initUI (sense w) (actuate w) (endlessLoop g)
