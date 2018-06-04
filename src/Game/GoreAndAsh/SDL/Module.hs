{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Game.GoreAndAsh.SDL.Module
Description : Monad transformer of the module
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains declaration of monad transformer of the core module and
instance for 'GameModule' class.
-}
module Game.GoreAndAsh.SDL.Module(
    SDLT(..)
  , runSDLT
  , withSDLT
  ) where

import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Proxy
import SDL

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL.State

-- | Monad transformer of SDL core module.
--
-- [@t@] - FRP engine implementation, can be almost always ignored.
--
-- How to embed module:
--
-- @
-- type AppMonad a = SDLT Spider (LoggingT Spider GMSpider) a
-- @
type SDLT t = ReaderT (SDLState t)

-- | Required initiatlization of library that should be put in main function
withSDLT :: IO a -> IO a
withSDLT io = do
  initializeAll
  liftIO $ putStrLn "SDL initialized"
  io

-- | Run SDLT layer
runSDLT :: MonadGame t m => SDLT t m a -> m a
runSDLT m = do
  (windowShownEvent, fireWindowShownEvent) <- newTriggerEvent
  (windowHiddenEvent, fireWindowHiddenEvent) <- newTriggerEvent
  (windowExposedEvent, fireWindowExposedEvent) <- newTriggerEvent
  (windowMovedEvent, fireWindowMovedEvent) <- newTriggerEvent
  (windowResizedEvent, fireWindowResizedEvent) <- newTriggerEvent
  (windowSizeChangedEvent, fireWindowSizeChangedEvent) <- newTriggerEvent
  (windowMinimizedEvent, fireWindowMinimizedEvent) <- newTriggerEvent
  (windowMaximizedEvent, fireWindowMaximizedEvent) <- newTriggerEvent
  (windowRestoredEvent, fireWindowRestoredEvent) <- newTriggerEvent
  (windowGainedMouseFocusEvent, fireWindowGainedMouseFocusEvent) <- newTriggerEvent
  (windowLostMouseFocusEvent, fireWindowLostMouseFocusEvent) <- newTriggerEvent
  (windowGainedKeyboardFocusEvent, fireWindowGainedKeyboardFocusEvent) <- newTriggerEvent
  (windowLostKeyboardFocusEvent, fireWindowLostKeyboardFocusEvent) <- newTriggerEvent
  (windowClosedEvent, fireWindowClosedEvent) <- newTriggerEvent
  (keyboardEvent, fireKeyboardEvent) <- newTriggerEvent
  (textEditingEvent, fireTextEditingEvent) <- newTriggerEvent
  (textInputEvent, fireTextInputEvent) <- newTriggerEvent
  (mouseMotionEvent, fireMouseMotionEvent) <- newTriggerEvent
  (mouseButtonEvent, fireMouseButtonEvent) <- newTriggerEvent
  (mouseWheelEvent, fireMouseWheelEvent) <- newTriggerEvent
  (joyAxisEvent, fireJoyAxisEvent) <- newTriggerEvent
  (joyBallEvent, fireJoyBallEvent) <- newTriggerEvent
  (joyHatEvent, fireJoyHatEvent) <- newTriggerEvent
  (joyButtonEvent, fireJoyButtonEvent) <- newTriggerEvent
  (joyDeviceEvent, fireJoyDeviceEvent) <- newTriggerEvent
  (controllerAxisEvent, fireControllerAxisEvent) <- newTriggerEvent
  (controllerButtonEvent, fireControllerButtonEvent) <- newTriggerEvent
  (controllerDeviceEvent, fireControllerDeviceEvent) <- newTriggerEvent
  (quitEvent, fireQuitEvent) <- newTriggerEvent
  (userEvent, fireUserEvent) <- newTriggerEvent
  (sysWMEvent, fireSysWMEvent) <- newTriggerEvent
  (touchFingerEvent, fireTouchFingerEvent) <- newTriggerEvent
  (multiGestureEvent, fireMultiGestureEvent) <- newTriggerEvent
  (dollarGestureEvent, fireDollarGestureEvent) <- newTriggerEvent
  (dropEvent, fireDropEvent) <- newTriggerEvent
  (clipboardUpdateEvent, fireClipboardUpdateEvent) <- newTriggerEvent

  let s = SDLState {
          sdlStateWindowShownEvent = windowShownEvent
        , sdlStateWindowHiddenEvent = windowHiddenEvent
        , sdlStateWindowExposedEvent = windowExposedEvent
        , sdlStateWindowMovedEvent = windowMovedEvent
        , sdlStateWindowResizedEvent = windowResizedEvent
        , sdlStateWindowSizeChangedEvent = windowSizeChangedEvent
        , sdlStateWindowMinimizedEvent = windowMinimizedEvent
        , sdlStateWindowMaximizedEvent = windowMaximizedEvent
        , sdlStateWindowRestoredEvent = windowRestoredEvent
        , sdlStateWindowGainedMouseFocusEvent = windowGainedMouseFocusEvent
        , sdlStateWindowLostMouseFocusEvent = windowLostMouseFocusEvent
        , sdlStateWindowGainedKeyboardFocusEvent = windowGainedKeyboardFocusEvent
        , sdlStateWindowLostKeyboardFocusEvent = windowLostKeyboardFocusEvent
        , sdlStateWindowClosedEvent = windowClosedEvent
        , sdlStateKeyboardEvent = keyboardEvent
        , sdlStateTextEditingEvent = textEditingEvent
        , sdlStateTextInputEvent = textInputEvent
        , sdlStateMouseMotionEvent = mouseMotionEvent
        , sdlStateMouseButtonEvent = mouseButtonEvent
        , sdlStateMouseWheelEvent = mouseWheelEvent
        , sdlStateJoyAxisEvent = joyAxisEvent
        , sdlStateJoyBallEvent = joyBallEvent
        , sdlStateJoyHatEvent = joyHatEvent
        , sdlStateJoyButtonEvent = joyButtonEvent
        , sdlStateJoyDeviceEvent = joyDeviceEvent
        , sdlStateControllerAxisEvent = controllerAxisEvent
        , sdlStateControllerButtonEvent = controllerButtonEvent
        , sdlStateControllerDeviceEvent = controllerDeviceEvent
        , sdlStateQuitEvent = quitEvent
        , sdlStateUserEvent = userEvent
        , sdlStateSysWMEvent = sysWMEvent
        , sdlStateTouchFingerEvent = touchFingerEvent
        , sdlStateMultiGestureEvent = multiGestureEvent
        , sdlStateDollarGestureEvent = dollarGestureEvent
        , sdlStateDropEvent = dropEvent
        , sdlStateClipboardUpdateEvent = clipboardUpdateEvent
        }

  -- Process single SDL event
  let handleEvent e = case e of
        WindowShownEvent d -> fireWindowShownEvent d
        WindowHiddenEvent d -> fireWindowHiddenEvent d
        WindowExposedEvent d -> fireWindowExposedEvent d
        WindowMovedEvent d -> fireWindowMovedEvent d
        WindowResizedEvent d -> fireWindowResizedEvent d
        WindowSizeChangedEvent d -> fireWindowSizeChangedEvent d
        WindowMinimizedEvent d -> fireWindowMinimizedEvent d
        WindowMaximizedEvent d -> fireWindowMaximizedEvent d
        WindowRestoredEvent d -> fireWindowRestoredEvent d
        WindowGainedMouseFocusEvent d -> fireWindowGainedMouseFocusEvent d
        WindowLostMouseFocusEvent d -> fireWindowLostMouseFocusEvent d
        WindowGainedKeyboardFocusEvent d -> fireWindowGainedKeyboardFocusEvent d
        WindowLostKeyboardFocusEvent d -> fireWindowLostKeyboardFocusEvent d
        WindowClosedEvent d -> fireWindowClosedEvent d
        KeyboardEvent d -> fireKeyboardEvent d
        TextEditingEvent d -> fireTextEditingEvent d
        TextInputEvent d -> fireTextInputEvent d
        MouseMotionEvent d -> fireMouseMotionEvent d
        MouseButtonEvent d -> fireMouseButtonEvent d
        MouseWheelEvent d -> fireMouseWheelEvent d
        JoyAxisEvent d -> fireJoyAxisEvent d
        JoyBallEvent d -> fireJoyBallEvent d
        JoyHatEvent d -> fireJoyHatEvent d
        JoyButtonEvent d -> fireJoyButtonEvent d
        JoyDeviceEvent d -> fireJoyDeviceEvent d
        ControllerAxisEvent d -> fireControllerAxisEvent d
        ControllerButtonEvent d -> fireControllerButtonEvent d
        ControllerDeviceEvent d -> fireControllerDeviceEvent d
        QuitEvent -> fireQuitEvent ()
        UserEvent d -> fireUserEvent d
        SysWMEvent d -> fireSysWMEvent d
        TouchFingerEvent d -> fireTouchFingerEvent d
        MultiGestureEvent d -> fireMultiGestureEvent d
        DollarGestureEvent d -> fireDollarGestureEvent d
        DropEvent d -> fireDropEvent d
        ClipboardUpdateEvent -> fireClipboardUpdateEvent ()
        _ -> return ()

  _ <- liftIO $ forkOS $ forever $ do
    e <- waitEvent
    void . handleEvent . eventPayload $ e
  runReaderT m s
