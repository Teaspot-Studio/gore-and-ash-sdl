{-|
Module      : Game.GoreAndAsh.SDL
Description : Module that contains SDL integration for Gore&Ash
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The core module contains API for SDL2 library integration. 
The module doesn't depends on others core modules and could be place in any place in 
game monad stack.

The module is NOT pure within first phase (see 'ModuleStack' docs), therefore currently only 'IO' end monad can handler the module.

Example of embedding:

@
-- | Application monad is monad stack build from given list of modules over base monad (IO)
type AppStack = ModuleStack [SDLT, ... other modules ... ] IO
newtype AppState = AppState (ModuleState AppStack)
  deriving (Generic)

instance NFData AppState 

-- | Wrapper around type family
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadSDL, ... other modules monads ... )
  
instance GameModule AppMonad AppState where 
  type ModuleState AppMonad = AppState
  runModule (AppMonad m) (AppState s) = do 
    (a, s') <- runModule m s 
    return (a, AppState s')
  newModuleState = AppState <$> newModuleState
  withModule _ = withModule (Proxy :: Proxy AppStack)
  cleanupModule (AppState s) = cleanupModule s 

-- | Arrow that is build over the monad stack
type AppWire a b = GameWire AppMonad a b
-- | Action that makes indexed app wire
type AppActor i a b = GameActor AppMonad i a b
@

-}
module Game.GoreAndAsh.SDL(
  -- * Low level API
    SDLState
  , SDLT
  , MonadSDL(..)
  -- * Arrow API
  , WindowConfig(..)
  , RendererConfig(..)
  , RendererType(..)
  , module ReExport
  -- ** Keyboard arrow API
  , keyScancode
  , keyPress
  , keyRelease
  , keyPressing
  -- ** Mouse arrow API
  , mouseScroll
  , mouseScrollX
  , mouseScrollY
  , mouseClick
  -- ** Window arrow API
  , windowClosed
  ) where

import SDL as ReExport hiding (get, Event)

import Game.GoreAndAsh.SDL.API as X 
import Game.GoreAndAsh.SDL.Module as X 
import Game.GoreAndAsh.SDL.State as X 