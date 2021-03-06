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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Game.GoreAndAsh.SDL.API(
  -- * Basic API
    MonadSDL(..)
  , WindowConfig(..)
  , RendererConfig(..)
  , RendererType(..)
  , module ReExport
  -- * High-level API wrappers
  , keyScancode
  , keyPress
  , keyRelease
  , keyPressing
  , mouseScroll
  , mouseScrollX
  , mouseScrollY
  , mouseButtonEvent
  , mouseClick
  , mouseRelease
  , mousePosition
  , mousePress
  , mouseClickPress
  , createMainWindow
  ) where

import Control.Concurrent
import Control.Lens ((^.), (&), (.~))
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Int
import Linear
import Linear.Affine

import SDL as ReExport hiding (get, Event, delay)

import Game.GoreAndAsh
import Game.GoreAndAsh.Time
import Game.GoreAndAsh.SDL.Module
import Game.GoreAndAsh.SDL.State
import Game.GoreAndAsh.SDL.Window

-- | API of the module
class (MonadIO m, MonadGame t m, MonadFix m) => MonadSDL t m | m -> t where
  -- | Creates new window widget
  sdlCreateWindow :: WindowWidgetConf t -> m (Event t (WindowWidget t))

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
  sdlClipboardUpdateEvent :: m (Event t ())

instance {-# OVERLAPPING #-} (MonadIO m, MonadGame t m) => MonadSDL t (SDLT t m) where
  sdlCreateWindow cfg@WindowWidgetConf{..} = do
    initTitle <- sample (current _windowCfgTitle)
    buildE <- getPostBuild
    -- First we need to create window in main thread
    we <- performInMainThread $ ffor buildE $ const $ do
      w <- createWindow initTitle _windowCfgConfig
      (mc, mr) <- case windowOpenGL _windowCfgConfig of
        Nothing ->  (,) <$> pure Nothing <*> (fmap Just $ createRenderer w (-1) _windowCfgRendererConfig)
        Just _ -> (,) <$> (fmap Just $ glCreateContext w) <*> pure Nothing
      pure (w, mc, mr)
    -- When we got window and context we can now initialize FRP network for it
    performNetwork $ ffor we $ \(w, _windowContext, mr) -> do
      -- Destroy context, renderer and window itself
      performEvent_ $ ffor _windowCfgDestroy $ const $ do
        whenJust _windowContext glDeleteContext
        whenJust mr destroyRenderer
        destroyWindow w

      -- Select context (if any), perform draw. Swapping users do theirself
      _windowDrawn <- performInMainThread $ ffor _windowCfgDraw $ \draw -> draw w mr

      performEvent_ $ ffor (updated _windowCfgTitle) (windowTitle w $=)
      performEvent_ $ ffor _windowCfgHide $ const $ hideWindow w
      performEvent_ $ ffor _windowCfgRaise $ const $ raiseWindow w
      performEvent_ $ ffor _windowCfgShow $ const $ showWindow w
      performEvent_ $ ffor _windowCfgMinimumSize (windowMinimumSize w $=)
      performEvent_ $ ffor _windowCfgMaximumSize (windowMaximumSize w $=)
      performEvent_ $ ffor _windowCfgSize (windowSize w $=)
      performEvent_ $ ffor _windowCfgBordered (windowBordered w $=)
      performEvent_ $ ffor _windowCfgBrightness (windowBrightness w $=)
      performEvent_ $ ffor _windowCfgGammaRamp (windowGammaRamp w $=)
      performEvent_ $ ffor _windowCfgGrab (windowGrab w $=)
      performEvent_ $ ffor _windowCfgWindowMode (setWindowMode w)
      performEvent_ $ ffor _windowCfgPosition (setWindowPosition w)

      -- Transforms and filters event
      let filterEvent :: Functor f => (a -> Window) -> (a -> b) -> f (Event t a) -> f (Event t b)
          filterEvent getter f = fmap (fmap f . ffilter ((== w) . getter))

          filterEventM :: Functor f => (a -> Maybe Window) -> (a -> b) -> f (Event t a) -> f (Event t b)
          filterEventM getter f = fmap (fmap f . ffilter ((== Just w) . getter))

      _windowShown <- filterEvent windowShownEventWindow (const ()) sdlWindowShownEvent
      _windowHidden <- filterEvent windowHiddenEventWindow (const ()) sdlWindowHiddenEvent
      _windowExposed <- filterEvent windowExposedEventWindow (const ()) sdlWindowExposedEvent
      _windowMoved <- filterEvent windowMovedEventWindow (fmap fromIntegral . windowMovedEventPosition) sdlWindowMovedEvent
      _windowResized <- filterEvent windowResizedEventWindow (fmap fromIntegral . windowResizedEventSize) sdlWindowResizedEvent
      _windowSizeChanged <- filterEvent windowSizeChangedEventWindow (const ()) sdlWindowSizeChangedEvent
      _windowMinimized <- filterEvent windowMinimizedEventWindow (const ()) sdlWindowMinimizedEvent
      _windowMaximized <- filterEvent windowMaximizedEventWindow (const ()) sdlWindowMaximizedEvent
      _windowRestored <- filterEvent windowRestoredEventWindow (const ()) sdlWindowRestoredEvent
      _windowGainedMouseFocus <- filterEvent windowGainedMouseFocusEventWindow (const ()) sdlWindowGainedMouseFocusEvent
      _windowLostMouseFocus <- filterEvent windowLostMouseFocusEventWindow (const ()) sdlWindowLostMouseFocusEvent
      _windowGainedKeyboardFocus <- filterEvent windowGainedKeyboardFocusEventWindow (const ()) sdlWindowGainedKeyboardFocusEvent
      _windowLostKeyboardFocus <- filterEvent windowLostKeyboardFocusEventWindow (const ()) sdlWindowLostKeyboardFocusEvent
      _windowClosed <- filterEvent windowClosedEventWindow (const ()) sdlWindowClosedEvent
      _windowKeyboardEvent <- filterEventM keyboardEventWindow id sdlKeyboardEvent
      _windowTextEditingEvent <- filterEventM textEditingEventWindow id sdlTextEditingEvent
      _windowTextInputEvent <- filterEventM textInputEventWindow id sdlTextInputEvent
      _windowMouseMotionEvent <- filterEventM mouseMotionEventWindow id sdlMouseMotionEvent
      _windowMouseButtonEvent <- filterEventM mouseButtonEventWindow id sdlMouseButtonEvent
      _windowMouseWheelEvent <- filterEventM mouseWheelEventWindow id sdlMouseWheelEvent
      _windowUserEvent <- filterEventM userEventWindow id sdlUserEvent

      let initialSize = fmap fromIntegral . windowInitialSize $ _windowCfgConfig
      _windowSizeDyn <- holdDyn initialSize _windowResized

      let _windowWindow = w
          _windowRenderer = mr
          _windowConf = cfg
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

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadGame t (mt m), MonadFix (mt m), MonadSDL t m, MonadTrans mt) => MonadSDL t (mt m) where
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
keyScancode w sc im = ffilter isNeeded $ _windowKeyboardEvent w
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
  pressE = ffilter isNeeded $ _windowKeyboardEvent w

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
mouseScroll w = mouseWheelEventPos <$> _windowMouseWheelEvent w

-- | Returns accumulated mouse scroll scince last frame
mouseScrollX :: Reflex t => WindowWidget t -> Event t Int32
mouseScrollX = fmap (^. _x) . mouseScroll

-- | Returns accumulated mouse scroll scince last frame
mouseScrollY :: Reflex t => WindowWidget t -> Event t Int32
mouseScrollY = fmap (^. _y) . mouseScroll

-- | Transform coordinates to [-1 .. 1] range.
convertCoords :: (Reflex t, MonadSample t m) => WindowWidget t -> Point V2 Int32 -> m (V2 Double)
convertCoords win (P (V2 xi yi)) = do
  V2 w h <- sample (current $ _windowSizeDyn win)
  return $ inv33 (viewportTransform2D 0 (V2 (fromIntegral w) (fromIntegral h)))
    `applyTransform2D`
    V2 (fromIntegral xi) (fromIntegral yi)

-- | Fires when user press or release mouse button within given window.
-- Click coordinates are in [-1 .. 1] range.
mouseButtonEvent :: Reflex t => WindowWidget t -> MouseButton -> InputMotion -> Event t (V2 Double)
mouseButtonEvent win mb motion = pushAlways (convertCoords win) $ fmap mouseButtonEventPos btnE
  where
    btnE = ffilter isNeeded $ _windowMouseButtonEvent win
    isNeeded MouseButtonEventData{..} = mouseButtonEventButton == mb && mouseButtonEventMotion == motion

-- | Fires when user clicks within window. Click coordinates are in [-1 .. 1] range
mouseClick :: Reflex t => WindowWidget t -> MouseButton -> Event t (V2 Double)
mouseClick win mb = mouseButtonEvent win mb Pressed

-- | Fires when user releases mouse button. Click coordinates are in [-1 .. 1] range
mouseRelease :: Reflex t => WindowWidget t -> MouseButton -> Event t (V2 Double)
mouseRelease win mb = mouseButtonEvent win mb Released

-- | Dynamic of mouse position in window. Coordinates are in [-1 .. 1] range
mousePosition :: (MonadHold t m, Reflex t) => WindowWidget t -> m (Dynamic t (V2 Double))
mousePosition win = holdDyn 0 (pushAlways (convertCoords win) posE)
  where
    posE = mouseMotionEventPos <$> _windowMouseMotionEvent win

-- | Return dynamic that contains positions of cursor between press and release of mouse button
mousePress :: (MonadHold t m, Reflex t) => WindowWidget t -> MouseButton -> m (Dynamic t (Maybe (V2 Double)))
mousePress w mb = holdDyn Nothing (leftmost [posE, clickE, releaseE])
  where
    clickE = Just <$> mouseClick w mb
    releaseE = const Nothing <$> mouseRelease w mb
    posE = flip pushAlways (_windowMouseMotionEvent w) $ \MouseMotionEventData{..} ->
      if mb `elem` mouseMotionEventState
        then Just <$> convertCoords w mouseMotionEventPos
        else return Nothing

-- | Generate event that fires with given rate when user holds mouse button
mouseClickPress :: MonadGame t m
  => WindowWidget t-- ^ Window the event is triggers for
  -> MouseButton -- ^ Which mouse button to track
  -> Int -- ^ How much occurences per second to do
  -> m (Event t (V2 Double))
mouseClickPress w mb fps = do
  pressDyn <- mousePress w mb
  oldPress <- delayWith Nothing pressDyn
  let
    startPressE = flip push (updated pressDyn) $ \mv -> do
      oldMv <- sample . current $ oldPress
      return $ case (oldMv, mv) of
        (Nothing, Just _) -> Just ()
        _ -> Nothing
    endPressE = flip push (updated pressDyn) $ \mv -> do
      oldMv <- sample . current $ oldPress
      return $ case (oldMv, mv) of
        (Just _, Nothing) -> Just ()
        _ -> Nothing
    tickDt = 1 / (fromIntegral fps :: Double)
    mkTick = tickEveryUntil (realToFrac tickDt) endPressE
  tickE <- switch . current <$> networkHold (pure never) (const mkTick <$> startPressE)
  let holdE = fmapMaybe id $ current pressDyn `tag` tickE
  return $ leftmost [holdE, mouseClick w mb]

-- | Create a main window with given initial confit, that is redrawn each time
-- the window is resized/maximized/restored/etc and if it is closed the application
-- gets signal to shutdown.
createMainWindow :: MonadSDL t m
  => Event t () -- ^ Window redraw event
  -> WindowDrawer -- ^ How to redraw window (including resizing and other additional causes of redraw)
  -> WindowWidgetConf t -- ^ Config to use
  -> m (Event t (WindowWidget t))
createMainWindow redrawE draw cfg = do
  buildE <- getPostBuild
  rec
    let cfg' = cfg
          & windowCfgDraw .~ fmap (const draw) drawE
          & windowCfgDestroy .~ closeE
    wE <- sdlCreateWindow cfg'
    closeE <- fmap (switch . current) $ holdDyn never $ _windowClosed <$> wE
    needRedrawE <- fmap (switch . current) $ holdDyn never $ windowNeedRedraw <$> wE
    let drawE = leftmost [needRedrawE, redrawE, buildE]

  postExitEvent $ leftmost [
      _windowCfgDestroy cfg
    , closeE]
  return wE
