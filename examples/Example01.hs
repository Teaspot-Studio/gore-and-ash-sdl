module Main where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Data.Proxy
import Foreign.C
import Game.GoreAndAsh.Core
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.SDL
import SDL (get)

type AppMonad = SDLT Spider (LoggingT Spider GMSpider)

drawFrame :: Window -> Maybe Renderer -> IO ()
drawFrame _ Nothing = pure ()
drawFrame win (Just r) = do
  rendererDrawColor r $= V4 0 0 0 0
  clear r
  rendererDrawColor r $= V4 250 0 0 0
  ws <- getCurrentSize
  let squareRect :: Rectangle Double
      squareRect = Rectangle (P $ V2 0.1 0.1) (V2 0.8 0.8)
  fillRect r (Just $ resizeRect ws squareRect)
  present r
  where
    getCurrentSize :: IO (V2 CInt)
    getCurrentSize = do
      vp <- get (rendererViewport r)
      case vp of
        Nothing -> return 0
        Just (Rectangle _ s) -> return s

    resizeRect :: V2 CInt -> Rectangle Double -> Rectangle CInt
    resizeRect (V2 vw vh) (Rectangle (P (V2 x y)) (V2 w h)) = Rectangle (P (V2 x' y')) (V2 w' h')
      where
        x' = round $ x * fromIntegral vw
        y' = round $ y * fromIntegral vh
        w' = round $ w * fromIntegral vw
        h' = round $ h * fromIntegral vh

app :: forall t m . MonadSDL t m => m ()
app = do
  _ <- createMainWindow never drawFrame defaultWindowCfg
  return ()

main :: IO ()
main = withSDLT $ runGM $ runLoggerT $ runSDLT (app :: AppMonad ())
