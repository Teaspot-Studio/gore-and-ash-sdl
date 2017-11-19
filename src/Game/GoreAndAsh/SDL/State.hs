{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.SDL.State
Description : Internal core module state
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Game.GoreAndAsh.SDL.State(
    SDLState(..)
  -- * Errors
  , SDL'ModuleException(..)
  , renderSDLModuleException
  ) where

import Control.Exception
import GHC.Generics (Generic)

import SDL.Event hiding (Event)

import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Logging

-- | Module specific exceptions (no exceptions at the moment)
data SDL'ModuleException = SDL'ModuleException
  deriving (Generic, Show)

instance Exception SDL'ModuleException

-- | User friendly message about module exception
renderSDLModuleException :: SDL'ModuleException -> LogStr
renderSDLModuleException _ = ""

-- | Inner state of SDL module.
--
-- [@t@] - FRP engine impelementation, almost always can be ignored.
data SDLState t = SDLState {
  sdlStateWindowShownEvent :: !(Event t WindowShownEventData)
, sdlStateWindowHiddenEvent :: !(Event t WindowHiddenEventData)
, sdlStateWindowExposedEvent :: !(Event t WindowExposedEventData)
, sdlStateWindowMovedEvent :: !(Event t WindowMovedEventData)
, sdlStateWindowResizedEvent :: !(Event t WindowResizedEventData)
, sdlStateWindowSizeChangedEvent :: !(Event t WindowSizeChangedEventData)
, sdlStateWindowMinimizedEvent :: !(Event t WindowMinimizedEventData)
, sdlStateWindowMaximizedEvent :: !(Event t WindowMaximizedEventData)
, sdlStateWindowRestoredEvent :: !(Event t WindowRestoredEventData)
, sdlStateWindowGainedMouseFocusEvent :: !(Event t WindowGainedMouseFocusEventData)
, sdlStateWindowLostMouseFocusEvent :: !(Event t WindowLostMouseFocusEventData)
, sdlStateWindowGainedKeyboardFocusEvent :: !(Event t WindowGainedKeyboardFocusEventData)
, sdlStateWindowLostKeyboardFocusEvent :: !(Event t WindowLostKeyboardFocusEventData)
, sdlStateWindowClosedEvent :: !(Event t WindowClosedEventData)
, sdlStateKeyboardEvent :: !(Event t KeyboardEventData)
, sdlStateTextEditingEvent :: !(Event t TextEditingEventData)
, sdlStateTextInputEvent :: !(Event t TextInputEventData)
, sdlStateMouseMotionEvent :: !(Event t MouseMotionEventData)
, sdlStateMouseButtonEvent :: !(Event t MouseButtonEventData)
, sdlStateMouseWheelEvent :: !(Event t MouseWheelEventData)
, sdlStateJoyAxisEvent :: !(Event t JoyAxisEventData)
, sdlStateJoyBallEvent :: !(Event t JoyBallEventData)
, sdlStateJoyHatEvent :: !(Event t JoyHatEventData)
, sdlStateJoyButtonEvent :: !(Event t JoyButtonEventData)
, sdlStateJoyDeviceEvent :: !(Event t JoyDeviceEventData)
, sdlStateControllerAxisEvent :: !(Event t ControllerAxisEventData)
, sdlStateControllerButtonEvent :: !(Event t ControllerButtonEventData)
, sdlStateControllerDeviceEvent :: !(Event t ControllerDeviceEventData)
, sdlStateQuitEvent :: !(Event t ())
, sdlStateUserEvent :: !(Event t UserEventData)
, sdlStateSysWMEvent :: !(Event t SysWMEventData)
, sdlStateTouchFingerEvent :: !(Event t TouchFingerEventData)
, sdlStateMultiGestureEvent :: !(Event t MultiGestureEventData)
, sdlStateDollarGestureEvent :: !(Event t DollarGestureEventData)
, sdlStateDropEvent :: !(Event t DropEventData)
, sdlStateClipboardUpdateEvent :: !(Event t ())
} deriving (Generic)
