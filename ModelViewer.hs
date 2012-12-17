module Main where 

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)
import qualified Loader as L
import qualified Texture as T
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toUpper)

data Action = Action (IO Action)

main = do
  -- invoke either active or passive drawing loop depending on command line argument
  args <- getArgs
  prog <- getProgName
  case args of 
    []      -> putStrLn "Please input image path" 
    path:xs -> main' path
 
main' path = do
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 1024 768) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "Image Viewer"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 0 0 0 0
  GL.clearDepth $= 1  -- enable depth clearing
  GL.texture Texture2D $= Enabled
 
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
    do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
  -- load file hierarchy
  filemap <- L.fullFileMap
  fileStr <- L.readPath filemap path
  let ext = fileExt path
  tex <- T.loadGLTexture fileStr ext 
  {-putStrLn $ show file-}

  -- run the main loop
  run tex
  -- finish up
  GLFW.closeWindow
  GLFW.terminate

-- we start with waitForPress action
run tex = loop waitForPress
  where 
 
    loop action = do
      -- draw the entire screen
      render tex
      -- swap buffer
      GLFW.swapBuffers
      -- check whether ESC is pressed for termination
      p <- GLFW.getKey GLFW.ESC
      unless (p == GLFW.Press) $
        do
            -- perform action
            Action action' <- action
            -- sleep for 1ms to yield CPU to other applications
            GLFW.sleep 0.001
 
            -- only continue when the window is not closed
            windowOpen <- GLFW.getParam GLFW.Opened
            unless (not windowOpen) $
              loop action' -- loop with next action
 
    waitForPress = do
      b <- GLFW.getMouseButton GLFW.ButtonLeft
      case b of
        GLFW.Release -> return (Action waitForPress)
        GLFW.Press   -> do
          -- when left mouse button is pressed, add the point
          -- to lines and switch to waitForRelease action.
          (GL.Position x y) <- GL.get GLFW.mousePos 
          return (Action waitForRelease)
 
    waitForRelease = do
        -- keep track of mouse movement while waiting for button 
        -- release
        (GL.Position x y) <- GL.get GLFW.mousePos
        -- update the line with new ending position
        b <- GLFW.getMouseButton GLFW.ButtonLeft
        case b of
          -- when button is released, switch back back to 
          -- waitForPress action
          GLFW.Release -> return (Action waitForPress)
          GLFW.Press   -> return (Action waitForRelease)

render tex = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.color $ color3 1 1 1
  GL.loadIdentity

  GL.textureBinding Texture2D $= Just tex
  GL.renderPrimitive GL.Quads $ do
       GL.texCoord (GL.TexCoord2 0 (0::GLfloat))
       GL.vertex (GL.Vertex3 (-1) (-1) (1::GLfloat)) -- bottom left of quad (Front)
       GL.texCoord (GL.TexCoord2 1 (0::GLfloat))
       GL.vertex (GL.Vertex3 1 (-1) (1::GLfloat)) -- bottom right of quad (Front)
       GL.texCoord (GL.TexCoord2 1 (1::GLfloat))
       GL.vertex (GL.Vertex3 1 1 (1::GLfloat)) -- top right of quad (Front)
       GL.texCoord (GL.TexCoord2 0 (1::GLfloat))
       GL.vertex (GL.Vertex3 (-1) 1 (1::GLfloat)) -- top left of quad (Front) 
 
fileExt filename = map toUpper $ drop (length filename - length "xxx") filename
 
vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3
