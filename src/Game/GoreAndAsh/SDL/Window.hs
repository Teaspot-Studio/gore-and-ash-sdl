{-|
Module      : Game.GoreAndAsh.SDL.Window
Description : SDL window widget
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE TemplateHaskell #-}
module Game.GoreAndAsh.SDL.Window(
  -- * Window widget
    WindowDrawer
  -- ** Window configuration
  , WindowWidgetConf(..)
  , defaultWindowCfg
  , windowCfgTitle
  , windowCfgConfig
  , windowCfgRendererConfig
  , windowCfgCreateContext
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
  -- ** Window widget output
  , WindowWidget(..)
  , windowWindow
  , windowRenderer
  , windowContextCreated
  , windowDrawn
  , windowConf
  , windowShown
  , windowHidden
  , windowExposed
  , windowMoved
  , windowResized
  , windowSizeChanged
  , windowSizeDyn
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
  , windowNeedRedraw
  ) where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Data.Word
import Foreign.C
import GHC.Generics
import Linear
import Linear.Affine
import Data.Vector.Storable as VS (Vector)

import SDL as ReExport hiding (get, Event)

import Game.GoreAndAsh

-- | Action that draws content of the window
type WindowDrawer t = Window -> Renderer -> HostFrame t ()

-- | Input configuration of window widget
data WindowWidgetConf t = WindowWidgetConf {
  -- | Window displayed text
  _windowCfgTitle :: Dynamic t Text
  -- | Static configuration of window
, _windowCfgConfig :: WindowConfig
  -- | Static configuration of window renderer
, _windowCfgRendererConfig :: RendererConfig
  -- | When to destroy the window
, _windowCfgDestroy :: Event t ()
  -- | When the window GL context is created (implementation takes only the first occurence of the event)
, _windowCfgCreateContext :: Event t ()
  -- | How to draw the window, each time the event fires the window is redrawn
, _windowCfgDraw :: Event t (WindowDrawer t)
  -- | The window is hidden when the event fires
, _windowCfgHide :: Event t ()
  -- | The window is raised above other windows and set input focus when the event fires
, _windowCfgRaise :: Event t ()
  -- | The window is shown when the event fires
, _windowCfgShow :: Event t ()
  -- | The window minimum size is changed when the event fires
, _windowCfgMinimumSize :: Event t (V2 CInt)
  -- | The window maximum size is changed when the event fires
, _windowCfgMaximumSize :: Event t (V2 CInt)
  -- | The window size is changed when the event fires
, _windowCfgSize :: Event t (V2 CInt)
  -- | The window border is updated when the event fires
, _windowCfgBordered :: Event t Bool
  -- | The window brightness property is updated when the event fires
, _windowCfgBrightness :: Event t Float
  -- | The window gamma ramp is updated when the event fires
, _windowCfgGammaRamp :: Event t (V3 (VS.Vector Word16))
  -- | When the event fires the window will be updated whether the mouse shall be confined to the window.
, _windowCfgGrab :: Event t Bool
  -- | When the event fires the window changes its window mode.
, _windowCfgWindowMode :: Event t WindowMode
  -- | When the event fires the window changes its position.
, _windowCfgPosition :: Event t WindowPosition
} deriving (Generic)

makeLenses ''WindowWidgetConf

-- | Return default window config
--
-- Note: by default window is resizeable
defaultWindowCfg :: Reflex t => WindowWidgetConf t
defaultWindowCfg = WindowWidgetConf {
    _windowCfgTitle = pure "NewWindow"
  , _windowCfgConfig = defaultWindow {
      windowResizable = True
    }
  , _windowCfgRendererConfig = defaultRenderer
  , _windowCfgDestroy = never
  , _windowCfgCreateContext = never
  , _windowCfgDraw = never
  , _windowCfgHide = never
  , _windowCfgRaise = never
  , _windowCfgShow = never
  , _windowCfgMinimumSize = never
  , _windowCfgMaximumSize = never
  , _windowCfgSize = never
  , _windowCfgBordered = never
  , _windowCfgBrightness = never
  , _windowCfgGammaRamp = never
  , _windowCfgGrab = never
  , _windowCfgWindowMode = never
  , _windowCfgPosition = never
  }

-- | Output of window widget with all outcoming events that the window supports.
data WindowWidget t = WindowWidget {
  -- | Window SDL object
  _windowWindow :: Window
  -- | Window SDL renderer
, _windowRenderer :: Renderer
  -- | Configuration that was used to create the window
, _windowConf :: WindowWidgetConf t
  -- | Tracks current size of window
, _windowSizeDyn :: Dynamic t (V2 Int)
  -- | Fired when a GL context is created for the window
, _windowContextCreated :: Event t GLContext
  -- | Fires when the window is rerendered
, _windowDrawn :: Event t ()
  -- | Fires when the window is shown
, _windowShown :: Event t ()
  -- | Fires when the window is hidden
, _windowHidden :: Event t ()
  -- | Fires when the window is exposed
, _windowExposed :: Event t ()
  -- | Fires when the window is moved
, _windowMoved :: Event t (Point V2 Int)
  -- | Fires when the window is resized
, _windowResized :: Event t (V2 Int)
  -- | The window size has changed, either as a result of an API call or through the system or user changing the window size; this event is followed by WindowResizedEvent if the size was changed by an external event, i.e. the user or the window manager.
, _windowSizeChanged :: Event t ()
  -- | Fires when the window is minimized
, _windowMinimized :: Event t ()
  -- | Fires when the window is maximized
, _windowMaximized :: Event t ()
  -- | The window has been restored to normal size and position.
, _windowRestored :: Event t ()
  -- | The window has gained mouse focus.
, _windowGainedMouseFocus :: Event t ()
  -- | The window lost mouse focus
, _windowLostMouseFocus :: Event t ()
  -- | The window has gained keyboard focus
, _windowGainedKeyboardFocus :: Event t ()
  -- | The window has lost keyboard focus
, _windowLostKeyboardFocus :: Event t ()
  -- | The window manager requests that the window be closed.
, _windowClosed :: Event t ()

  -- | A keyboard key has been pressed or released for the window.
, _windowKeyboardEvent :: Event t KeyboardEventData
  -- | Keyboard text editing is occured for the window.
, _windowTextEditingEvent :: Event t TextEditingEventData
  -- | Keyboard text is inputed for the window.
, _windowTextInputEvent :: Event t TextInputEventData

  -- | A mouse or pointer device was moved over the window
, _windowMouseMotionEvent :: Event t MouseMotionEventData
  -- | A mouse or pointer device button was pressed or released.
, _windowMouseButtonEvent :: Event t MouseButtonEventData
  -- | A mouse wheel event for the window.
, _windowMouseWheelEvent :: Event t MouseWheelEventData

  -- | User defined (external to Haskell) event is occured
, _windowUserEvent :: Event t UserEventData
} deriving (Generic)

makeLenses ''WindowWidget

-- | Get event that fires when window widget need a redraw
windowNeedRedraw :: Reflex t => WindowWidget t -> Event t ()
windowNeedRedraw w = leftmost [
    nulify $ _windowResized w
  , nulify $ _windowShown w
  , nulify $ _windowExposed w
  , nulify $ _windowContextCreated w
  , nulify $ _windowExposed w
  , nulify $ _windowRestored w
  ]
  where
    nulify = fmap (const ())
