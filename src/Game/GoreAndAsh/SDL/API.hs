{-|
Module      : Game.GoreAndAsh.SDL.API
Description : Monadic and arrow API for SDL core module
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The module contains monadic and arrow API of the core module.
-}
module Game.GoreAndAsh.SDL.API(
    MonadSDL(..)
  , WindowConfig(..)
  , RendererConfig(..)
  , RendererType(..)
  , module ReExport
  -- * Window widget
  , WindowDrawer
  -- ** Window configuration
  , WindowWidgetConf
  , defaultWindowCfg
  , windowCfgTitle
  , windowCfgConfig
  , windowCfgRendererConfig
  , windowCfgDestroy
  , windowCfgDraw
  , windowCfgHide
  , windowCfgRaise
  , windowCfgShow
  , windowCfgMinimumSize
  , windowCfgMaximumSize
  , windowCfgSize
  , windowCfgBordered
  , windowCfgBrightness
  , windowCfgGammaRamp
  , windowCfgGrab
  , windowCfgWindowMode
  , windowCfgPosition
  -- ** Window widget
  , WindowWidget
  , windowWindow
  , windowRenderer
  , windowShown
  , windowHidden
  , windowExposed
  , windowMoved
  , windowResized
  , windowSizeChanged
  , windowMinimized
  , windowMaximized
  , windowRestored
  , windowGainedMouseFocus
  , windowLostMouseFocus
  , windowGainedKeyboardFocus
  , windowLostKeyboardFocus
  , windowClosed
  , windowSysWMEvent
  , windowKeyboardEvent
  , windowTextEditingEvent
  , windowTextInputEvent
  , windowMouseMotionEvent
  , windowMouseButtonEvent
  , windowMouseWheelEvent
  , windowUserEvent
  -- * Keyboard arrow API
  , keyScancode
  , keyPress
  , keyRelease
  , keyPressing
  -- * Mouse arrow API
  , mouseScroll
  , mouseScrollX
  , mouseScrollY
  , mouseClick
  -- * Window arrow API
  , windowClosed
  ) where

import Control.Lens ((^.))
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Int
import Data.Sequence (Seq)
import Data.Text (Text, unpack)
import Data.Word
import Foreign
import GHC.Generics
import Linear
import Linear.Affine
import Prelude hiding (id, (.))
import qualified Data.HashMap.Strict as H
import qualified Data.Sequence as S

import SDL as ReExport hiding (get, Event)
import SDL.Internal.Types
import qualified SDL.Raw as SDLRaw

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL.Module
import Game.GoreAndAsh.SDL.State

instance Exception SDL'ModuleException

-- | Action that draws content of the window
type WindowDrawer t = Window -> Renderer -> HostFrame t ()

-- | Input configuration of window widget
data WindowWidgetConf t = WindowWidgetConf {
  -- | Window displayed text
  windowCfgTitle :: !(Dynamic t Text)
  -- | Static configuration of window
, windowCfgConfig :: !WindowConfig
  -- | Static configuration of window renderer
, windowCfgRendererConfig :: !RendererConfig
  -- | When to destroy the window
, windowCfgDestroy :: !(Event t ())
  -- | When the window GL context is created (implementation takes only the first occurence of the event)
, windowCfgCreateContext :: !(Event t ())
  -- | How to draw the window, each time the event fires the window is redrawn
, windowCfgDraw :: !(Event t (WindowDrawer t))
  -- | The window is hidden when the event fires
, windowCfgHide :: !(Event t ())
  -- | The window is raised above other windows and set input focus when the event fires
, windowCfgRaise :: !(Event t ())
  -- | The window is shown when the event fires
, windowCfgShow :: !(Event t ())
  -- | The window minimum size is changed when the event fires
, windowCfgMinimumSize :: !(Event t (V2 CInt))
  -- | The window maximum size is changed when the event fires
, windowCfgMaximumSize :: !(Event t (V2 CInt))
  -- | The window size is changed when the event fires
, windowCfgSize :: !(Event t (V2 CInt))
  -- | The window border is updated when the event fires
, windowCfgBordered :: !(Event t Bool)
  -- | The window brightness property is updated when the event fires
, windowCfgBrightness :: !(Event t Float)
  -- | The window gamma ramp is updated when the event fires
, windowCfgGammaRamp :: !(Event t (V3 (Vector Word16)))
  -- | When the event fires the window will be updated whether the mouse shall be confined to the window.
, windowCfgGrab :: !(Event t Bool)
  -- | When the event fires the window changes its window mode.
, windowCfgWindowMode :: !(Event t WindowMode)
  -- | When the event fires the window changes its position.
, windowCfgPosition :: !(Event t WindowPosition)
}

-- | Return default window config
defaultWindowCfg :: Reflex t => WindowWidgetConf t
defaultWindowCfg = WindowWidgetConf {
    windowCfgTitle = pure "NewWindow"
  , windowCfgConfig = defaultWindow
  , windowCfgRendererConfig = defaultRenderer
  , windowCfgDestroy = never
  , windowCfgDraw = never
  , windowCfgHide = never
  , windowCfgRaise = never
  , windowCfgShow = never
  , windowCfgMinimumSize = never
  , windowCfgMaximumSize = never
  , windowCfgSize = never
  , windowCfgBordered = never
  , windowCfgBrightness = never
  , windowCfgGammaRamp = never
  , windowCfgGrab = never
  , windowCfgWindowMode = never
  , windowCfgPosition = never
  }

-- | Output of window widget with all outcoming events that the window supports.
data WindowWidget t = WindowWidget {
  -- | Window SDL object
  windowWindow :: !Window
  -- | Window SDL renderer
, windowRenderer :: !Renderer

  -- | Fires when the window is shown
, windowShown :: !(Event t ())
  -- | Fires when the window is hidden
, windowHidden :: !(Event t ())
  -- | Fires when the window is exposed
, windowExposed :: !(Event t ())
  -- | Fires when the window is moved
, windowMoved :: !(Event t (Point V2 Int32))
  -- | Fires when the window is resized
, windowResized :: !(Event t (V2 Int32))
  -- | The window size has changed, either as a result of an API call or through the system or user changing the window size; this event is followed by WindowResizedEvent if the size was changed by an external event, i.e. the user or the window manager.
, windowSizeChanged :: !(Event t ())
  -- | Fires when the window is minimized
, windowMinimized :: !(Event t ())
  -- | Fires when the window is maximized
, windowMaximized :: !(Event t ())
  -- | The window has been restored to normal size and position.
, windowRestored :: !(Event t ())
  -- | The window has gained mouse focus.
, windowGainedMouseFocus :: !(Event t ())
  -- | The window lost mouse focus
, windowLostMouseFocus :: !(Event t ())
  -- | The window has gained keyboard focus
, windowGainedKeyboardFocus :: !(Event t ())
  -- | The window has lost keyboard focus
, windowLostKeyboardFocus :: !(Event t ())
  -- | The window manager requests that the window be closed.
, windowClosed :: !(Event t ())
  -- | A video driver dependent system event
, windowSysWMEvent :: !(Event t SysWMmsg)

  -- | A keyboard key has been pressed or released for the window.
, windowKeyboardEvent :: !(Event t KeyboardEventData)
  -- | Keyboard text editing is occured for the window.
, windowTextEditingEvent :: !(Event t TextEditingEventData)
  -- | Keyboard text is inputed for the window.
, windowTextInputEvent :: !(Event t TextInputEventData)

  -- | A mouse or pointer device was moved over the window
, windowMouseMotionEvent :: !(Event t MouseMotionEventData)
  -- | A mouse or pointer device button was pressed or released.
, windowMouseButtonEvent :: !(Event t MouseButtonEventData)
  -- | A mouse wheel event for the window.
, windowMouseWheelEvent :: !(Event t MouseWheelEventData)

  -- | User defined (external to Haskell) event is occured
, windowUserEvent :: !(Event t UserEventData)
}

-- | API of the module
class (MonadIO m, MonadError SDL'ModuleException m) => MonadSDL t m | m -> t where
  -- | Creates window and stores in module context
  --
  -- Throws `SDL'ConflictingWindows`on name conflict
  sdlCreateWindow :: WindowWidgetConf t -> m (WindowWidget t)

  -- | Getting window shown event
  sdlWindowShownEvent :: m (Event t WindowShownEventData)
  -- | Getting window hidden event
  sdlWindowHiddenEvent :: m (Event t WindowHiddenEventData)
  -- | Getting window exposed event
  sdlWindowExposedEvent :: m (Event t WindowExposedEventData)
  -- | Getting window move event
  sdlWindowMovedEvent :: m (Event t WindowMovedEventData)
  -- | Getting window resize event
  --
  -- This is event is always preceded by WindowSizeChangedEvent.
  sdlWindowResizedEvent :: m (Event t WindowResizedEventData)
  -- | Getting window resize event
  --
  -- The window size has changed, either as a result of an API call or through the system or user changing the window size; this event is followed by WindowResizedEvent if the size was changed by an external event, i.e. the user or the window manager.
  sdlWindowSizeChangedEvent :: m (Event t WindowSizeChangedEventData)
  -- | Getting window minimization event
  sdlWindowMinimizedEvent :: m (Event t WindowMinimizedEventData)
  -- | Getting window maximization event
  sdlWindowMaximizedEvent :: m (Event t WindowMaximizedEventData)
  -- | Getting window restore event
  sdlWindowRestoredEvent :: m (Event t WindowRestoredEventData)
  -- | Getting window focus event
  sdlWindowGainedMouseFocusEvent :: m (Event t WindowGainedMouseFocusEventData)
  -- | Getting window focus event
  sdlWindowLostMouseFocusEvent :: m (Event t WindowLostMouseFocusEventData)
  -- | Getting window focus event
  sdlWindowGainedKeyboardFocusEvent :: m (Event t WindowGainedKeyboardFocusEventData)
  -- | Getting window focus event
  sdlWindowLostKeyboardFocusEvent :: m (Event t WindowLostKeyboardFocusEventData)
  -- | Getting window close event
  sdlWindowClosedEvent :: m (Event t WindowClosedEventData)

  -- | Getting keyboard event
  sdlKeyboardEvent :: m (Event t KeyboardEventData)
  -- | Getting input API event
  sdlTextEditingEvent :: m (Event t TextEditingEventData)
  -- | Getting input API event
  sdlTextInputEvent :: m (Event t TextInputEventData)

  -- | Getting mouse event
  sdlMouseMotionEvent :: m (Event t MouseMotionEventData)
  -- | Getting mouse event
  sdlMouseButtonEvent :: m (Event t MouseButtonEventData)
  -- | Getting mouse event
  sdlMouseWheelEvent :: m (Event t MouseWheelEventData)

  -- | Getting joystick event
  sdlJoyAxisEvent :: m (Event t JoyAxisEventData)
  -- | Getting joystick event
  sdlJoyBallEvent :: m (Event t JoyBallEventData)
  -- | Getting joystick event
  sdlJoyHatEvent :: m (Event t JoyHatEventData)
  -- | Getting joystick event
  sdlJoyButtonEvent :: m (Event t JoyButtonEventData)
  -- | Getting joystick event
  sdlJoyDeviceEvent :: m (Event t JoyDeviceEventData)

  -- | Getting controller event
  sdlControllerAxisEvent :: m (Event t ControllerAxisEventData)
  -- | Getting controller event
  sdlControllerButtonEvent :: m (Event t ControllerButtonEventData)
  -- | Getting controller event
  sdlControllerDeviceEvent :: m (Event t ControllerDeviceEventData)

  -- | Getting quit request event
  sdlQuitEvent :: m (Event t ())
  -- | Getting user event
  sdlUserEvent :: m (Event t UserEventData)
  -- | Getting video driver specific event
  sdlSysWMEvent :: m (Event t SysWMEventData)

  -- | Getting touch event
  sdlTouchFingerEvent :: m (Event t TouchFingerEventData)
  -- | Getting touch event
  sdlMultiGestureEvent :: m (Event t MultiGestureEventData)
  -- | Getting touch event
  sdlDollarGestureEvent :: m (Event t DollarGestureEventData)

  -- | Getting file opened event
  sdlDropEvent :: m (Event t DropEventData)
  -- | Getting clipboard changed event
  sdlClipboardUpdateEvent :: m (Event t ClipboardUpdateEventData)

instance {-# OVERLAPPING #-} (MonadIO m, MonadThrow m) => MonadSDL (SDLT s m) where
  sdlCreateWindowM n t wc rc = do
    w <- createWindow t wc
    r <- createRenderer w (-1) rc
    s <- get
    case H.lookup n . sdlWindows $! s of
      Just _ -> throwM . SDL'ConflictingWindows $! n
      Nothing -> do
        let winfo = WindowInfo {
                winfoWindow = w
              , winfoRenderer = r
              , winfoColor = Nothing
              , winfoContext = Nothing
              }
        SDLT . put $! s {
            sdlWindows = H.insert n winfo . sdlWindows $! s
          }
        return (w, r)

  sdlGetWindowM n = do
    s <- SDLT get
    return . fmap (\WindowInfo{..} -> (winfoWindow, winfoRenderer)) . H.lookup n . sdlWindows $! s

  sdlDestroyWindowM n = do
    s <- SDLT get
    case H.lookup n . sdlWindows $! s of
      Just WindowInfo{..} -> do
        destroyRenderer winfoRenderer
        destroyWindow winfoWindow
        whenJust winfoContext glDeleteContext
        SDLT . put $! s {
          sdlWindows = H.delete n . sdlWindows $! s
        }
      Nothing -> return ()

  sdlSetBackColor n c = do
    s <- SDLT get
    case H.lookup n . sdlWindows $! s of
      Just winfo -> SDLT . put $! s {
          sdlWindows = H.insert n winfo' . sdlWindows $! s
        }
        where
          winfo' = winfo { winfoColor = c }
      Nothing -> return ()

  sdlCreateContext n = do
    s <- SDLT get
    case H.lookup n . sdlWindows $! s of
      Just winfo -> do
        whenJust (winfoContext winfo) glDeleteContext
        cntx <- glCreateContext $ winfoWindow winfo
        let winfo' = winfo { winfoContext = Just cntx }
        SDLT . put $! s {
          sdlWindows = H.insert n winfo' . sdlWindows $! s
        }
        liftIO . putStrLn $! "Created context for " <> unpack n
      Nothing -> return ()

  sdlMakeCurrent n = do
    s <- SDLT get
    case H.lookup n . sdlWindows $! s of
      Just WindowInfo{..} -> whenJust winfoContext $ glMakeCurrent winfoWindow
      Nothing -> return ()

  sdlWindowShownEventsM = sdlWindowShownEvents <$> get
  sdlWindowHiddenEventsM = sdlWindowHiddenEvents <$> get
  sdlWindowExposedEventsM = sdlWindowExposedEvents <$> get
  sdlWindowMovedEventsM = sdlWindowMovedEvents <$> get
  sdlWindowResizedEventsM = sdlWindowResizedEvents <$> get
  sdlWindowSizeChangedEventsM = sdlWindowSizeChangedEvents <$> get
  sdlWindowMinimizedEventsM = sdlWindowMinimizedEvents <$> get
  sdlWindowMaximizedEventsM = sdlWindowMaximizedEvents <$> get
  sdlWindowRestoredEventsM = sdlWindowRestoredEvents <$> get
  sdlWindowGainedMouseFocusEventsM = sdlWindowGainedMouseFocusEvents <$> get
  sdlWindowLostMouseFocusEventsM = sdlWindowLostMouseFocusEvents <$> get
  sdlWindowGainedKeyboardFocusEventsM = sdlWindowGainedKeyboardFocusEvents <$> get
  sdlWindowLostKeyboardFocusEventsM = sdlWindowLostKeyboardFocusEvents <$> get
  sdlWindowClosedEventsM = sdlWindowClosedEvents <$> get
  sdlKeyboardEventsM = sdlKeyboardEvents <$> get
  sdlTextEditingEventsM = sdlTextEditingEvents <$> get
  sdlTextInputEventsM = sdlTextInputEvents <$> get
  sdlMouseMotionEventsM = sdlMouseMotionEvents <$> get
  sdlMouseButtonEventsM = sdlMouseButtonEvents <$> get
  sdlMouseWheelEventsM = sdlMouseWheelEvents <$> get
  sdlJoyAxisEventsM = sdlJoyAxisEvents <$> get
  sdlJoyBallEventsM = sdlJoyBallEvents <$> get
  sdlJoyHatEventsM = sdlJoyHatEvents <$> get
  sdlJoyButtonEventsM = sdlJoyButtonEvents <$> get
  sdlJoyDeviceEventsM = sdlJoyDeviceEvents <$> get
  sdlControllerAxisEventsM = sdlControllerAxisEvents <$> get
  sdlControllerButtonEventsM = sdlControllerButtonEvents <$> get
  sdlControllerDeviceEventsM = sdlControllerDeviceEvents <$> get
  sdlQuitEventM = sdlQuitEvent <$> get
  sdlUserEventsM = sdlUserEvents <$> get
  sdlSysWMEventsM = sdlSysWMEvents <$> get
  sdlTouchFingerEventsM = sdlTouchFingerEvents <$> get
  sdlMultiGestureEventsM = sdlMultiGestureEvents <$> get
  sdlDollarGestureEventsM = sdlDollarGestureEvents <$> get
  sdlDropEventsM = sdlDropEvents <$> get
  sdlClipboardUpdateEventsM = sdlClipboardUpdateEvents <$> get

  {-# INLINE sdlCreateWindow #-}
  {-# INLINE sdlWindowShownEventsM #-}
  {-# INLINE sdlWindowHiddenEventsM #-}
  {-# INLINE sdlWindowExposedEventsM #-}
  {-# INLINE sdlWindowMovedEventsM #-}
  {-# INLINE sdlWindowResizedEventsM #-}
  {-# INLINE sdlWindowSizeChangedEventsM #-}
  {-# INLINE sdlWindowMinimizedEventsM #-}
  {-# INLINE sdlWindowMaximizedEventsM #-}
  {-# INLINE sdlWindowRestoredEventsM #-}
  {-# INLINE sdlWindowGainedMouseFocusEventsM #-}
  {-# INLINE sdlWindowLostMouseFocusEventsM #-}
  {-# INLINE sdlWindowGainedKeyboardFocusEventsM #-}
  {-# INLINE sdlWindowLostKeyboardFocusEventsM #-}
  {-# INLINE sdlWindowClosedEventsM #-}
  {-# INLINE sdlKeyboardEventsM #-}
  {-# INLINE sdlTextEditingEventsM #-}
  {-# INLINE sdlTextInputEventsM #-}
  {-# INLINE sdlMouseMotionEventsM #-}
  {-# INLINE sdlMouseButtonEventsM #-}
  {-# INLINE sdlMouseWheelEventsM #-}
  {-# INLINE sdlJoyAxisEventsM #-}
  {-# INLINE sdlJoyBallEventsM #-}
  {-# INLINE sdlJoyHatEventsM #-}
  {-# INLINE sdlJoyButtonEventsM #-}
  {-# INLINE sdlJoyDeviceEventsM #-}
  {-# INLINE sdlControllerAxisEventsM #-}
  {-# INLINE sdlControllerButtonEventsM #-}
  {-# INLINE sdlControllerDeviceEventsM #-}
  {-# INLINE sdlQuitEventM #-}
  {-# INLINE sdlUserEventsM #-}
  {-# INLINE sdlSysWMEventsM #-}
  {-# INLINE sdlTouchFingerEventsM #-}
  {-# INLINE sdlMultiGestureEventsM #-}
  {-# INLINE sdlDollarGestureEventsM #-}
  {-# INLINE sdlDropEventsM #-}
  {-# INLINE sdlClipboardUpdateEventsM #-}

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadThrow (mt m), MonadSDL m, MonadTrans mt) => MonadSDL (mt m) where
  sdlCreateWindow cfg = lift $ sdlCreateWindow cfg

  sdlWindowShownEventsM = lift sdlWindowShownEventsM
  sdlWindowHiddenEventsM = lift sdlWindowHiddenEventsM
  sdlWindowExposedEventsM = lift sdlWindowExposedEventsM
  sdlWindowMovedEventsM = lift sdlWindowMovedEventsM
  sdlWindowResizedEventsM = lift sdlWindowResizedEventsM
  sdlWindowSizeChangedEventsM = lift sdlWindowSizeChangedEventsM
  sdlWindowMinimizedEventsM = lift sdlWindowMinimizedEventsM
  sdlWindowMaximizedEventsM = lift sdlWindowMaximizedEventsM
  sdlWindowRestoredEventsM = lift sdlWindowRestoredEventsM
  sdlWindowGainedMouseFocusEventsM = lift sdlWindowGainedMouseFocusEventsM
  sdlWindowLostMouseFocusEventsM = lift sdlWindowLostMouseFocusEventsM
  sdlWindowGainedKeyboardFocusEventsM = lift sdlWindowGainedKeyboardFocusEventsM
  sdlWindowLostKeyboardFocusEventsM = lift sdlWindowLostKeyboardFocusEventsM
  sdlWindowClosedEventsM = lift sdlWindowClosedEventsM
  sdlKeyboardEventsM = lift sdlKeyboardEventsM
  sdlTextEditingEventsM = lift sdlTextEditingEventsM
  sdlTextInputEventsM = lift sdlTextInputEventsM
  sdlMouseMotionEventsM = lift sdlMouseMotionEventsM
  sdlMouseButtonEventsM = lift sdlMouseButtonEventsM
  sdlMouseWheelEventsM = lift sdlMouseWheelEventsM
  sdlJoyAxisEventsM = lift sdlJoyAxisEventsM
  sdlJoyBallEventsM = lift sdlJoyBallEventsM
  sdlJoyHatEventsM = lift sdlJoyHatEventsM
  sdlJoyButtonEventsM = lift sdlJoyButtonEventsM
  sdlJoyDeviceEventsM = lift sdlJoyDeviceEventsM
  sdlControllerAxisEventsM = lift sdlControllerAxisEventsM
  sdlControllerButtonEventsM = lift sdlControllerButtonEventsM
  sdlControllerDeviceEventsM = lift sdlControllerDeviceEventsM
  sdlQuitEventM = lift sdlQuitEventM
  sdlUserEventsM = lift sdlUserEventsM
  sdlSysWMEventsM = lift sdlSysWMEventsM
  sdlTouchFingerEventsM = lift sdlTouchFingerEventsM
  sdlMultiGestureEventsM = lift sdlMultiGestureEventsM
  sdlDollarGestureEventsM = lift sdlDollarGestureEventsM
  sdlDropEventsM = lift sdlDropEventsM
  sdlClipboardUpdateEventsM = lift sdlClipboardUpdateEventsM

  {-# INLINE sdlCreateWindow #-}
  {-# INLINE sdlWindowShownEventsM #-}
  {-# INLINE sdlWindowHiddenEventsM #-}
  {-# INLINE sdlWindowExposedEventsM #-}
  {-# INLINE sdlWindowMovedEventsM #-}
  {-# INLINE sdlWindowResizedEventsM #-}
  {-# INLINE sdlWindowSizeChangedEventsM #-}
  {-# INLINE sdlWindowMinimizedEventsM #-}
  {-# INLINE sdlWindowMaximizedEventsM #-}
  {-# INLINE sdlWindowRestoredEventsM #-}
  {-# INLINE sdlWindowGainedMouseFocusEventsM #-}
  {-# INLINE sdlWindowLostMouseFocusEventsM #-}
  {-# INLINE sdlWindowGainedKeyboardFocusEventsM #-}
  {-# INLINE sdlWindowLostKeyboardFocusEventsM #-}
  {-# INLINE sdlWindowClosedEventsM #-}
  {-# INLINE sdlKeyboardEventsM #-}
  {-# INLINE sdlTextEditingEventsM #-}
  {-# INLINE sdlTextInputEventsM #-}
  {-# INLINE sdlMouseMotionEventsM #-}
  {-# INLINE sdlMouseButtonEventsM #-}
  {-# INLINE sdlMouseWheelEventsM #-}
  {-# INLINE sdlJoyAxisEventsM #-}
  {-# INLINE sdlJoyBallEventsM #-}
  {-# INLINE sdlJoyHatEventsM #-}
  {-# INLINE sdlJoyButtonEventsM #-}
  {-# INLINE sdlJoyDeviceEventsM #-}
  {-# INLINE sdlControllerAxisEventsM #-}
  {-# INLINE sdlControllerButtonEventsM #-}
  {-# INLINE sdlControllerDeviceEventsM #-}
  {-# INLINE sdlQuitEventM #-}
  {-# INLINE sdlUserEventsM #-}
  {-# INLINE sdlSysWMEventsM #-}
  {-# INLINE sdlTouchFingerEventsM #-}
  {-# INLINE sdlMultiGestureEventsM #-}
  {-# INLINE sdlDollarGestureEventsM #-}
  {-# INLINE sdlDropEventsM #-}
  {-# INLINE sdlClipboardUpdateEventsM #-}

-- | Fires when specific scancode key is pressed/unpressed
keyScancode :: MonadSDL m => Scancode -> InputMotion -> GameWire m a (Event (Seq KeyboardEventData))
keyScancode sc im = liftGameMonad $ do
  es <- S.filter isNeeded <$> sdlKeyboardEventsM
  return $! if S.null es
    then NoEvent
    else Event es
  where
    isNeeded KeyboardEventData{..} = keyboardEventKeyMotion == im
      && sc == keysymScancode keyboardEventKeysym

-- | Fires when specific scancode key is pressed
keyPress :: MonadSDL m => Scancode -> GameWire m a (Event (Seq KeyboardEventData))
keyPress sc = keyScancode sc Pressed

-- | Fires when specific scancode key is released
keyRelease :: MonadSDL m => Scancode -> GameWire m a (Event (Seq KeyboardEventData))
keyRelease sc = keyScancode sc Released

-- | Fires event from moment of press until release of given key
keyPressing :: MonadSDL m => Scancode -> GameWire m a (Event KeyboardEventData)
keyPressing sc = go NoEvent
  where
    go !e = mkGen $ \_ _ -> do
      !mks <- S.viewr . S.filter isNeeded <$> sdlKeyboardEventsM
      return $! case mks of
        S.EmptyR -> (Right e, go e)
        _ S.:> mds@KeyboardEventData{..} -> case keyboardEventKeyMotion of
          Pressed -> (Right $! Event mds, go $! Event mds)
          Released -> (Right NoEvent, go NoEvent)

    isNeeded KeyboardEventData{..} = sc == keysymScancode keyboardEventKeysym

-- | Returns accumulated mouse scroll scince last frame
mouseScroll :: MonadSDL m => GameWire m a (Event (V2 Int32))
mouseScroll = liftGameMonad $ do
  es <- sdlMouseWheelEventsM
  return $! if S.null es
    then NoEvent
    else Event . sumV . fmap mouseWheelEventPos $! es

-- | Returns accumulated mouse scroll scince last frame
mouseScrollX :: MonadSDL m => GameWire m a (Event Int32)
mouseScrollX = mapE (^. _x) . mouseScroll

-- | Returns accumulated mouse scroll scince last frame
mouseScrollY :: MonadSDL m => GameWire m a (Event Int32)
mouseScrollY = mapE (^. _y) . mouseScroll

-- | Fires when window with specific name is closed
windowClosed :: MonadSDL m => Text -> GameWire m a (Event ())
windowClosed n = go Nothing
  where
  go Nothing = mkGen $ \_ _ -> do
    mr <- sdlGetWindowM n
    return $! case mr of
      Nothing -> (Right NoEvent, go Nothing)
      Just (w, _) -> (Right NoEvent, go $ Just w)
  go (Just w) = liftGameMonad $ do
    es <- S.filter isNeeded <$> sdlWindowClosedEventsM
    return $! if S.null es
      then NoEvent
      else Event ()
    where
      isNeeded WindowClosedEventData{..} = windowClosedEventWindow == w

-- | Fires when user clicks within window. Click coordinates are in [-1 .. 1] range
mouseClick :: MonadSDL m => MouseButton -> GameWire m a (Event (V2 Double))
mouseClick mb = liftGameMonad $ do
  es <- S.filter isNeeded <$> sdlMouseButtonEventsM
  case S.viewr es of
    S.EmptyR -> return NoEvent
    _ S.:> MouseButtonEventData{..} -> do
      (size :: V2 Int) <- getWindowSize mouseButtonEventWindow
      return . Event $! transformCoords size mouseButtonEventPos
  where
    isNeeded MouseButtonEventData{..} = mouseButtonEventButton == mb && mouseButtonEventMotion == Pressed
    transformCoords (V2 w h) (P (V2 xi yi)) =
      inv33 (viewportTransform2D 0 (V2 (fromIntegral w) (fromIntegral h)))
      `applyTransform2D`
      V2 (fromIntegral xi) (fromIntegral yi)

-- | Helper to hide pointer manipulation while getting window size
getWindowSize :: (MonadIO m, Integral a) => Window -> m (V2 a)
getWindowSize (Window wptr) = liftIO $ with 0 $ \xptr -> with 0 $ \yptr -> do
  SDLRaw.getWindowSize wptr xptr yptr
  x <- peek xptr
  y <- peek yptr
  return $! V2 (fromIntegral x) (fromIntegral y)