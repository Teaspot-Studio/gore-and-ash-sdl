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
  -- * Basic API
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
  , windowKeyboardEvent
  , windowTextEditingEvent
  , windowTextInputEvent
  , windowMouseMotionEvent
  , windowMouseButtonEvent
  , windowMouseWheelEvent
  , windowUserEvent
  -- * High-level API wrappers
  , keyScancode
  , keyPress
  , keyRelease
  , keyPressing
  , mouseScroll
  , mouseScrollX
  , mouseScrollY
  , mouseClick
  ) where

import Control.Lens ((^.))
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Extra (whenJust)
import Control.Monad.Reader
import Data.Int
import Data.Text (Text)
import Data.Word
import Foreign.C
import GHC.Generics
import Linear
import Linear.Affine
import Data.Vector.Storable as VS (Vector)

import SDL as ReExport hiding (get, Event)

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL.Module
import Game.GoreAndAsh.SDL.State

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
, windowCfgGammaRamp :: !(Event t (V3 (VS.Vector Word16)))
  -- | When the event fires the window will be updated whether the mouse shall be confined to the window.
, windowCfgGrab :: !(Event t Bool)
  -- | When the event fires the window changes its window mode.
, windowCfgWindowMode :: !(Event t WindowMode)
  -- | When the event fires the window changes its position.
, windowCfgPosition :: !(Event t WindowPosition)
} deriving (Generic)

-- | Return default window config
defaultWindowCfg :: Reflex t => WindowWidgetConf t
defaultWindowCfg = WindowWidgetConf {
    windowCfgTitle = pure "NewWindow"
  , windowCfgConfig = defaultWindow
  , windowCfgRendererConfig = defaultRenderer
  , windowCfgDestroy = never
  , windowCfgCreateContext = never
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
  -- | Configuration that was used to create the window
, windowConf :: !(WindowWidgetConf t)
  -- | Tracks current size of window
, windowSizeDyn :: !(Dynamic t (V2 Int))
  -- | Fired when a GL context is created for the window
, windowContextCreated :: !(Event t GLContext)
  -- | Fires when the window is rerendered
, windowDrawn :: !(Event t ())
  -- | Fires when the window is shown
, windowShown :: !(Event t ())
  -- | Fires when the window is hidden
, windowHidden :: !(Event t ())
  -- | Fires when the window is exposed
, windowExposed :: !(Event t ())
  -- | Fires when the window is moved
, windowMoved :: !(Event t (Point V2 Int))
  -- | Fires when the window is resized
, windowResized :: !(Event t (V2 Int))
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
} deriving (Generic)

-- | API of the module
class (MonadIO m, MonadError SDL'ModuleException m) => MonadSDL t m | m -> t where
  -- | Creates new window widget
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

instance {-# OVERLAPPING #-} (MonadIO m, MonadCatch m, MonadAppHost t m) => MonadSDL t (SDLT t m) where
  sdlCreateWindow cfg@WindowWidgetConf{..} = do
    initTitle <- sample (current windowCfgTitle)
    w <- createWindow initTitle windowCfgConfig
    r <- createRenderer w (-1) windowCfgRendererConfig

    -- Create context on demand and watch current value of it
    createContextEvent <- headE windowCfgCreateContext -- don't create twice
    windowContextCreated <- performEvent $ ffor createContextEvent $ const $ glCreateContext w
    contextB <- hold Nothing $ fmap Just windowContextCreated
    let whenContext m = do
          mcontext <- sample contextB
          whenJust mcontext m

    -- Destroy context, renderer and window itself
    performEvent_ $ ffor windowCfgDestroy $ const $ do
      whenContext glDeleteContext
      destroyRenderer r
      destroyWindow w

    -- Select context (if any), perform draw and then swap buffers
    windowDrawn <- performEvent $ ffor windowCfgDraw $ \draw -> do
      whenContext (glMakeCurrent w)
      draw w r
      glSwapWindow w

    performEvent_ $ ffor (updated windowCfgTitle) (windowTitle w $=)
    performEvent_ $ ffor windowCfgHide $ const $ hideWindow w
    performEvent_ $ ffor windowCfgRaise $ const $ raiseWindow w
    performEvent_ $ ffor windowCfgShow $ const $ showWindow w
    performEvent_ $ ffor windowCfgMinimumSize (windowMinimumSize w $=)
    performEvent_ $ ffor windowCfgMaximumSize (windowMaximumSize w $=)
    performEvent_ $ ffor windowCfgSize (windowSize w $=)
    performEvent_ $ ffor windowCfgBordered (windowBordered w $=)
    performEvent_ $ ffor windowCfgBrightness (windowBrightness w $=)
    performEvent_ $ ffor windowCfgGammaRamp (windowGammaRamp w $=)
    performEvent_ $ ffor windowCfgGrab (windowGrab w $=)
    performEvent_ $ ffor windowCfgWindowMode (setWindowMode w)
    performEvent_ $ ffor windowCfgPosition (setWindowPosition w)

    -- | Transforms and filters event
    let filterEvent :: Functor f => (a -> Window) -> (a -> b) -> f (Event t a) -> f (Event t b)
        filterEvent getter f = fmap (fmap f . ffilter ((== w) . getter))

    windowShown <- filterEvent windowShownEventWindow (const ()) sdlWindowShownEvent
    windowHidden <- filterEvent windowHiddenEventWindow (const ()) sdlWindowHiddenEvent
    windowExposed <- filterEvent windowExposedEventWindow (const ()) sdlWindowExposedEvent
    windowMoved <- filterEvent windowMovedEventWindow (fmap fromIntegral . windowMovedEventPosition) sdlWindowMovedEvent
    windowResized <- filterEvent windowResizedEventWindow (fmap fromIntegral . windowResizedEventSize) sdlWindowResizedEvent
    windowSizeChanged <- filterEvent windowSizeChangedEventWindow (const ()) sdlWindowSizeChangedEvent
    windowMinimized <- filterEvent windowMinimizedEventWindow (const ()) sdlWindowMinimizedEvent
    windowMaximized <- filterEvent windowMaximizedEventWindow (const ()) sdlWindowMaximizedEvent
    windowRestored <- filterEvent windowRestoredEventWindow (const ()) sdlWindowRestoredEvent
    windowGainedMouseFocus <- filterEvent windowGainedMouseFocusEventWindow (const ()) sdlWindowGainedMouseFocusEvent
    windowLostMouseFocus <- filterEvent windowLostMouseFocusEventWindow (const ()) sdlWindowLostMouseFocusEvent
    windowGainedKeyboardFocus <- filterEvent windowGainedKeyboardFocusEventWindow (const ()) sdlWindowGainedKeyboardFocusEvent
    windowLostKeyboardFocus <- filterEvent windowLostKeyboardFocusEventWindow (const ()) sdlWindowLostKeyboardFocusEvent
    windowClosed <- filterEvent windowClosedEventWindow (const ()) sdlWindowClosedEvent
    windowKeyboardEvent <- filterEvent keyboardEventWindow id sdlKeyboardEvent
    windowTextEditingEvent <- filterEvent textEditingEventWindow id sdlTextEditingEvent
    windowTextInputEvent <- filterEvent textInputEventWindow id sdlTextInputEvent
    windowMouseMotionEvent <- filterEvent mouseMotionEventWindow id sdlMouseMotionEvent
    windowMouseButtonEvent <- filterEvent mouseButtonEventWindow id sdlMouseButtonEvent
    windowMouseWheelEvent <- filterEvent mouseWheelEventWindow id sdlMouseWheelEvent
    windowUserEvent <- filterEvent userEventWindow id sdlUserEvent

    let initialSize = fmap fromIntegral . windowInitialSize $ windowCfgConfig
    windowSizeDyn <- holdDyn initialSize windowResized

    let windowWindow = w
        windowRenderer = r
        windowConf = cfg
    return WindowWidget{..}

  sdlWindowShownEvent = asks sdlStateWindowShownEvent
  sdlWindowHiddenEvent = asks sdlStateWindowHiddenEvent
  sdlWindowExposedEvent = asks sdlStateWindowExposedEvent
  sdlWindowMovedEvent = asks sdlStateWindowMovedEvent
  sdlWindowResizedEvent = asks sdlStateWindowResizedEvent
  sdlWindowSizeChangedEvent = asks sdlStateWindowSizeChangedEvent
  sdlWindowMinimizedEvent = asks sdlStateWindowMinimizedEvent
  sdlWindowMaximizedEvent = asks sdlStateWindowMaximizedEvent
  sdlWindowRestoredEvent = asks sdlStateWindowRestoredEvent
  sdlWindowGainedMouseFocusEvent = asks sdlStateWindowGainedMouseFocusEvent
  sdlWindowLostMouseFocusEvent = asks sdlStateWindowLostMouseFocusEvent
  sdlWindowGainedKeyboardFocusEvent = asks sdlStateWindowGainedKeyboardFocusEvent
  sdlWindowLostKeyboardFocusEvent = asks sdlStateWindowLostKeyboardFocusEvent
  sdlWindowClosedEvent = asks sdlStateWindowClosedEvent
  sdlKeyboardEvent = asks sdlStateKeyboardEvent
  sdlTextEditingEvent = asks sdlStateTextEditingEvent
  sdlTextInputEvent = asks sdlStateTextInputEvent
  sdlMouseMotionEvent = asks sdlStateMouseMotionEvent
  sdlMouseButtonEvent = asks sdlStateMouseButtonEvent
  sdlMouseWheelEvent = asks sdlStateMouseWheelEvent
  sdlJoyAxisEvent = asks sdlStateJoyAxisEvent
  sdlJoyBallEvent = asks sdlStateJoyBallEvent
  sdlJoyHatEvent = asks sdlStateJoyHatEvent
  sdlJoyButtonEvent = asks sdlStateJoyButtonEvent
  sdlJoyDeviceEvent = asks sdlStateJoyDeviceEvent
  sdlControllerAxisEvent = asks sdlStateControllerAxisEvent
  sdlControllerButtonEvent = asks sdlStateControllerButtonEvent
  sdlControllerDeviceEvent = asks sdlStateControllerDeviceEvent
  sdlQuitEvent = asks sdlStateQuitEvent
  sdlUserEvent = asks sdlStateUserEvent
  sdlSysWMEvent = asks sdlStateSysWMEvent
  sdlTouchFingerEvent = asks sdlStateTouchFingerEvent
  sdlMultiGestureEvent = asks sdlStateMultiGestureEvent
  sdlDollarGestureEvent = asks sdlStateDollarGestureEvent
  sdlDropEvent = asks sdlStateDropEvent
  sdlClipboardUpdateEvent = asks sdlStateClipboardUpdateEvent

  {-# INLINE sdlCreateWindow #-}
  {-# INLINE sdlWindowShownEvent #-}
  {-# INLINE sdlWindowHiddenEvent #-}
  {-# INLINE sdlWindowExposedEvent #-}
  {-# INLINE sdlWindowMovedEvent #-}
  {-# INLINE sdlWindowResizedEvent #-}
  {-# INLINE sdlWindowSizeChangedEvent #-}
  {-# INLINE sdlWindowMinimizedEvent #-}
  {-# INLINE sdlWindowMaximizedEvent #-}
  {-# INLINE sdlWindowRestoredEvent #-}
  {-# INLINE sdlWindowGainedMouseFocusEvent #-}
  {-# INLINE sdlWindowLostMouseFocusEvent #-}
  {-# INLINE sdlWindowGainedKeyboardFocusEvent #-}
  {-# INLINE sdlWindowLostKeyboardFocusEvent #-}
  {-# INLINE sdlWindowClosedEvent #-}
  {-# INLINE sdlKeyboardEvent #-}
  {-# INLINE sdlTextEditingEvent #-}
  {-# INLINE sdlTextInputEvent #-}
  {-# INLINE sdlMouseMotionEvent #-}
  {-# INLINE sdlMouseButtonEvent #-}
  {-# INLINE sdlMouseWheelEvent #-}
  {-# INLINE sdlJoyAxisEvent #-}
  {-# INLINE sdlJoyBallEvent #-}
  {-# INLINE sdlJoyHatEvent #-}
  {-# INLINE sdlJoyButtonEvent #-}
  {-# INLINE sdlJoyDeviceEvent #-}
  {-# INLINE sdlControllerAxisEvent #-}
  {-# INLINE sdlControllerButtonEvent #-}
  {-# INLINE sdlControllerDeviceEvent #-}
  {-# INLINE sdlQuitEvent #-}
  {-# INLINE sdlUserEvent #-}
  {-# INLINE sdlSysWMEvent #-}
  {-# INLINE sdlTouchFingerEvent #-}
  {-# INLINE sdlMultiGestureEvent #-}
  {-# INLINE sdlDollarGestureEvent #-}
  {-# INLINE sdlDropEvent #-}
  {-# INLINE sdlClipboardUpdateEvent #-}

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadError SDL'ModuleException (mt m), MonadSDL t m, MonadTrans mt) => MonadSDL t (mt m) where
  sdlCreateWindow cfg = lift $ sdlCreateWindow cfg

  sdlWindowShownEvent = lift sdlWindowShownEvent
  sdlWindowHiddenEvent = lift sdlWindowHiddenEvent
  sdlWindowExposedEvent = lift sdlWindowExposedEvent
  sdlWindowMovedEvent = lift sdlWindowMovedEvent
  sdlWindowResizedEvent = lift sdlWindowResizedEvent
  sdlWindowSizeChangedEvent = lift sdlWindowSizeChangedEvent
  sdlWindowMinimizedEvent = lift sdlWindowMinimizedEvent
  sdlWindowMaximizedEvent = lift sdlWindowMaximizedEvent
  sdlWindowRestoredEvent = lift sdlWindowRestoredEvent
  sdlWindowGainedMouseFocusEvent = lift sdlWindowGainedMouseFocusEvent
  sdlWindowLostMouseFocusEvent = lift sdlWindowLostMouseFocusEvent
  sdlWindowGainedKeyboardFocusEvent = lift sdlWindowGainedKeyboardFocusEvent
  sdlWindowLostKeyboardFocusEvent = lift sdlWindowLostKeyboardFocusEvent
  sdlWindowClosedEvent = lift sdlWindowClosedEvent
  sdlKeyboardEvent = lift sdlKeyboardEvent
  sdlTextEditingEvent = lift sdlTextEditingEvent
  sdlTextInputEvent = lift sdlTextInputEvent
  sdlMouseMotionEvent = lift sdlMouseMotionEvent
  sdlMouseButtonEvent = lift sdlMouseButtonEvent
  sdlMouseWheelEvent = lift sdlMouseWheelEvent
  sdlJoyAxisEvent = lift sdlJoyAxisEvent
  sdlJoyBallEvent = lift sdlJoyBallEvent
  sdlJoyHatEvent = lift sdlJoyHatEvent
  sdlJoyButtonEvent = lift sdlJoyButtonEvent
  sdlJoyDeviceEvent = lift sdlJoyDeviceEvent
  sdlControllerAxisEvent = lift sdlControllerAxisEvent
  sdlControllerButtonEvent = lift sdlControllerButtonEvent
  sdlControllerDeviceEvent = lift sdlControllerDeviceEvent
  sdlQuitEvent = lift sdlQuitEvent
  sdlUserEvent = lift sdlUserEvent
  sdlSysWMEvent = lift sdlSysWMEvent
  sdlTouchFingerEvent = lift sdlTouchFingerEvent
  sdlMultiGestureEvent = lift sdlMultiGestureEvent
  sdlDollarGestureEvent = lift sdlDollarGestureEvent
  sdlDropEvent = lift sdlDropEvent
  sdlClipboardUpdateEvent = lift sdlClipboardUpdateEvent

  {-# INLINE sdlCreateWindow #-}
  {-# INLINE sdlWindowShownEvent #-}
  {-# INLINE sdlWindowHiddenEvent #-}
  {-# INLINE sdlWindowExposedEvent #-}
  {-# INLINE sdlWindowMovedEvent #-}
  {-# INLINE sdlWindowResizedEvent #-}
  {-# INLINE sdlWindowSizeChangedEvent #-}
  {-# INLINE sdlWindowMinimizedEvent #-}
  {-# INLINE sdlWindowMaximizedEvent #-}
  {-# INLINE sdlWindowRestoredEvent #-}
  {-# INLINE sdlWindowGainedMouseFocusEvent #-}
  {-# INLINE sdlWindowLostMouseFocusEvent #-}
  {-# INLINE sdlWindowGainedKeyboardFocusEvent #-}
  {-# INLINE sdlWindowLostKeyboardFocusEvent #-}
  {-# INLINE sdlWindowClosedEvent #-}
  {-# INLINE sdlKeyboardEvent #-}
  {-# INLINE sdlTextEditingEvent #-}
  {-# INLINE sdlTextInputEvent #-}
  {-# INLINE sdlMouseMotionEvent #-}
  {-# INLINE sdlMouseButtonEvent #-}
  {-# INLINE sdlMouseWheelEvent #-}
  {-# INLINE sdlJoyAxisEvent #-}
  {-# INLINE sdlJoyBallEvent #-}
  {-# INLINE sdlJoyHatEvent #-}
  {-# INLINE sdlJoyButtonEvent #-}
  {-# INLINE sdlJoyDeviceEvent #-}
  {-# INLINE sdlControllerAxisEvent #-}
  {-# INLINE sdlControllerButtonEvent #-}
  {-# INLINE sdlControllerDeviceEvent #-}
  {-# INLINE sdlQuitEvent #-}
  {-# INLINE sdlUserEvent #-}
  {-# INLINE sdlSysWMEvent #-}
  {-# INLINE sdlTouchFingerEvent #-}
  {-# INLINE sdlMultiGestureEvent #-}
  {-# INLINE sdlDollarGestureEvent #-}
  {-# INLINE sdlDropEvent #-}
  {-# INLINE sdlClipboardUpdateEvent #-}

-- | Fires when specific scancode key is pressed/unpressed
keyScancode :: Reflex t => WindowWidget t -> Scancode -> InputMotion -> Event t KeyboardEventData
keyScancode w sc im = ffilter isNeeded $ windowKeyboardEvent w
  where
  isNeeded KeyboardEventData{..} = keyboardEventKeyMotion == im
    && sc == keysymScancode keyboardEventKeysym

-- | Fires when specific scancode key is pressed
keyPress :: Reflex t => WindowWidget t -> Scancode -> Event t KeyboardEventData
keyPress w sc = keyScancode w sc Pressed

-- | Fires when specific scancode key is released
keyRelease :: Reflex t => WindowWidget t -> Scancode -> Event t KeyboardEventData
keyRelease w sc = keyScancode w sc Released

-- | Fires event from moment of press until release of given key
keyPressing :: (MonadHold t m, MonadFix m, Reflex t) => WindowWidget t -> Scancode -> m (Dynamic t (Maybe KeyboardEventData))
keyPressing w sc = foldDynMaybe checkRelease Nothing pressE
  where
  pressE = ffilter isNeeded $ windowKeyboardEvent w

  checkRelease mds@KeyboardEventData{..} mold = case mold of
    Nothing -> case keyboardEventKeyMotion of
      Pressed -> Just (Just mds)
      Released -> Nothing
    Just _ -> case keyboardEventKeyMotion of
      Pressed -> Nothing
      Released -> Just Nothing

  isNeeded KeyboardEventData{..} = sc == keysymScancode keyboardEventKeysym

-- | Returns accumulated mouse scroll scince last frame
mouseScroll :: Reflex t => WindowWidget t -> Event t (V2 Int32)
mouseScroll w = mouseWheelEventPos <$> windowMouseWheelEvent w

-- | Returns accumulated mouse scroll scince last frame
mouseScrollX :: Reflex t => WindowWidget t -> Event t Int32
mouseScrollX = fmap (^. _x) . mouseScroll

-- | Returns accumulated mouse scroll scince last frame
mouseScrollY :: Reflex t => WindowWidget t -> Event t Int32
mouseScrollY = fmap (^. _y) . mouseScroll

-- | Fires when user clicks within window. Click coordinates are in [-1 .. 1] range
mouseClick :: Reflex t => WindowWidget t -> MouseButton -> Event t (V2 Double)
mouseClick widg mb = pushAlways convertCoords btnE
  where
    btnE = ffilter isNeeded $ windowMouseButtonEvent widg
    isNeeded MouseButtonEventData{..} = mouseButtonEventButton == mb && mouseButtonEventMotion == Pressed

    convertCoords MouseButtonEventData{..} = do
      size <- sample (current $ windowSizeDyn widg)
      return $ transformCoords size mouseButtonEventPos

    transformCoords (V2 w h) (P (V2 xi yi)) =
      inv33 (viewportTransform2D 0 (V2 (fromIntegral w) (fromIntegral h)))
      `applyTransform2D`
      V2 (fromIntegral xi) (fromIntegral yi)
