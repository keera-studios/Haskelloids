module Haskelloids.Input (UserInput(..),
                          inputInit,
                          getWindowEvs,
                          procWindowEvs
                         )where

-- import graphics
import Graphics.HGL.Window (Window, Event(..), maybeGetWindowEvent)
import Graphics.HGL.Key (Key(..), isLeftKey, isRightKey, isUpKey, isShiftLKey,
                         isCharKey, keyToChar)

-- import Yampa
import qualified FRP.Yampa as FRP (Event(..))

-- #### Data type definitions ##################################################

data UserInput = UserInput { -- events that correspond to user input
  uiLeftClick  :: FRP.Event (),
  uiFire       :: FRP.Event (),
  uiHyperSpace :: FRP.Event (),
  uiTurnLeft   :: FRP.Event Bool,
  uiTurnRight  :: FRP.Event Bool,
  uiThrust     :: FRP.Event Bool
}

-- inputInit - the user-input at time t=0
inputInit :: UserInput
inputInit = UserInput {
  uiLeftClick  = FRP.NoEvent,
  uiFire       = FRP.NoEvent,
  uiHyperSpace = FRP.NoEvent,
  uiTurnLeft   = FRP.NoEvent,
  uiTurnRight  = FRP.NoEvent,
  uiThrust     = FRP.NoEvent
}

-- getWindowEvs - collect all events since last window tick
getWindowEvs :: Window -> IO [Event]
getWindowEvs w = do e <- maybeGetWindowEvent w
                    case e of Just x  -> do es <- getWindowEvs w
                                            return (x : es)
                              Nothing -> return []

-- procWindowEvs - process the window events passed and apply to the passed game input
procWindowEvs :: UserInput -> [Event] -> UserInput
procWindowEvs gi evs = foldl procWindowEv gi evs

procWindowEv :: UserInput -> Event -> UserInput
procWindowEv gi Button{ isLeft = left, isDown = down }
  | left && down = gi{ uiLeftClick  = FRP.Event ()   }
procWindowEv gi Key{ keysym = k, isDown = down }
  | isLeftKey   k          = gi{ uiTurnLeft   = FRP.Event down }
  | isRightKey  k          = gi{ uiTurnRight  = FRP.Event down }
  | isUpKey     k          = gi{ uiThrust     = FRP.Event down }
  | isShiftLKey k
     && down               = gi{ uiHyperSpace = FRP.Event ()   }
  | isCharKey   k 
     && keyToChar k == ' '
     && down               = gi{ uiFire       = FRP.Event ()   }
procWindowEv gi _ = gi
