import qualified Graphics.UI.SDL as Sdl
import Prelude
import qualified Graphics.Rendering.OpenGL as Ogl
import Data.List
import Data.Maybe

main = do
  Sdl.init [Sdl.InitVideo]
  Sdl.setVideoMode 640 480 16 [Sdl.OpenGL]
  Ogl.viewport Ogl.$= (Ogl.Position 0 0, Ogl.Size 640 480)
  Ogl.clearColor Ogl.$= (Ogl.Color4 0 1.0 0 0)

  scr <- Sdl.getVideoSurface
  mainGameLoop scr

  Sdl.quit


getEvents = do
  ev <- Sdl.pollEvent
  nextEvents ev
    where
      nextEvents Sdl.NoEvent = return []
      nextEvents x = do
              ev <- Sdl.pollEvent
              xs <- nextEvents ev
              return ( x: xs)

mainGameLoop w = do
  Ogl.clear [Ogl.ColorBuffer]
  Ogl.flush
  Sdl.glSwapBuffers

  {-xs <- getEvents-}

  {-if isJust $ find (==Sdl.Quit) xs-}
    {-then return ()-}
    {-else mainGameLoop w-}

  mainGameLoop w
