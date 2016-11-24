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
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import Data.Proxy
import SDL

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL.State

-- | Monad transformer of SDL core module.
--
-- [@t@] - FRP engine implementation, can be almost always ignored.
--
-- [@m@] - Next monad in modules monad stack;
--
-- [@a@] - Type of result value;
--
-- How to embed module:
--
-- @
-- newtype AppMonad t a = AppMonad (SDLT t (LoggingT (GameMonad t)) a)
--   deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadSDL t, LoggingMonad, MonadSample t, MonadHold t)
-- @
newtype SDLT t m a = SDLT { runSDLT :: ReaderT (SDLState t) m a }
  deriving (Functor, Applicative, Monad, MonadReader (SDLState t), MonadFix
    , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadSample t, MonadHold t)

instance MonadTrans (SDLT t) where
  lift = SDLT . lift

instance MonadCatch m => MonadError SDL'ModuleException (SDLT t m) where
  throwError = throwM
  catchError = catch

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (SDLT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (SDLT t m) where
  subscribeEvent = lift . subscribeEvent

instance MonadAppHost t m => MonadAppHost t (SDLT t m) where
  getFireAsync = lift getFireAsync
  getRunAppHost = do
    runner <- SDLT getRunAppHost
    return $ \m -> runner $ runSDLT m
  performPostBuild_ = lift . performPostBuild_
  liftHostFrame = lift . liftHostFrame

instance MonadBase IO m => MonadBase IO (SDLT t m) where
  liftBase = SDLT . liftBase

instance MonadResource m => MonadResource (SDLT t m) where
  liftResourceT = SDLT . liftResourceT

instance (MonadIO (HostFrame t), GameModule t m) => GameModule t (SDLT t m) where
  type ModuleOptions t (SDLT t m) = ModuleOptions t m

  runModule opts m = do
    (windowShownEvent, fireWindowShownEvent) <- newExternalEvent
    (windowHiddenEvent, fireWindowHiddenEvent) <- newExternalEvent
    (windowExposedEvent, fireWindowExposedEvent) <- newExternalEvent
    (windowMovedEvent, fireWindowMovedEvent) <- newExternalEvent
    (windowResizedEvent, fireWindowResizedEvent) <- newExternalEvent
    (windowSizeChangedEvent, fireWindowSizeChangedEvent) <- newExternalEvent
    (windowMinimizedEvent, fireWindowMinimizedEvent) <- newExternalEvent
    (windowMaximizedEvent, fireWindowMaximizedEvent) <- newExternalEvent
    (windowRestoredEvent, fireWindowRestoredEvent) <- newExternalEvent
    (windowGainedMouseFocusEvent, fireWindowGainedMouseFocusEvent) <- newExternalEvent
    (windowLostMouseFocusEvent, fireWindowLostMouseFocusEvent) <- newExternalEvent
    (windowGainedKeyboardFocusEvent, fireWindowGainedKeyboardFocusEvent) <- newExternalEvent
    (windowLostKeyboardFocusEvent, fireWindowLostKeyboardFocusEvent) <- newExternalEvent
    (windowClosedEvent, fireWindowClosedEvent) <- newExternalEvent
    (keyboardEvent, fireKeyboardEvent) <- newExternalEvent
    (textEditingEvent, fireTextEditingEvent) <- newExternalEvent
    (textInputEvent, fireTextInputEvent) <- newExternalEvent
    (mouseMotionEvent, fireMouseMotionEvent) <- newExternalEvent
    (mouseButtonEvent, fireMouseButtonEvent) <- newExternalEvent
    (mouseWheelEvent, fireMouseWheelEvent) <- newExternalEvent
    (joyAxisEvent, fireJoyAxisEvent) <- newExternalEvent
    (joyBallEvent, fireJoyBallEvent) <- newExternalEvent
    (joyHatEvent, fireJoyHatEvent) <- newExternalEvent
    (joyButtonEvent, fireJoyButtonEvent) <- newExternalEvent
    (joyDeviceEvent, fireJoyDeviceEvent) <- newExternalEvent
    (controllerAxisEvent, fireControllerAxisEvent) <- newExternalEvent
    (controllerButtonEvent, fireControllerButtonEvent) <- newExternalEvent
    (controllerDeviceEvent, fireControllerDeviceEvent) <- newExternalEvent
    (quitEvent, fireQuitEvent) <- newExternalEvent
    (userEvent, fireUserEvent) <- newExternalEvent
    (sysWMEvent, fireSysWMEvent) <- newExternalEvent
    (touchFingerEvent, fireTouchFingerEvent) <- newExternalEvent
    (multiGestureEvent, fireMultiGestureEvent) <- newExternalEvent
    (dollarGestureEvent, fireDollarGestureEvent) <- newExternalEvent
    (dropEvent, fireDropEvent) <- newExternalEvent
    (clipboardUpdateEvent, fireClipboardUpdateEvent) <- newExternalEvent

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

    -- | Process single SDL event
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
          ClipboardUpdateEvent d -> fireClipboardUpdateEvent d
          _ -> return False

    liftIO $ do
      es <- pollEvents
      mapM_ (void . handleEvent) (eventPayload <$> es)
    runModule opts (runReaderT (runSDLT m) s)

  withModule t _ io = do
    initializeAll
    liftIO $ putStrLn "SDL initialized"
    withModule t (Proxy :: Proxy m) io
