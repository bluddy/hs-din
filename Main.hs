module Main where 

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)
import qualified Loader as L
import qualified Reader as R
import qualified Texture as T
import Graphics.Rendering.OpenGL.Raw.EXT.TextureCompressionS3tc
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)

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
 
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
    do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
  -- keep all line strokes as a list of points in an IORef
  lines <- newIORef []
  -- load file hierarchy
  filemap <- L.fullFileMap
  fileStr <- L.readPath filemap path
  texName <- loadGLTextures fileStr
  {-putStrLn $ show file-}

  -- run the main loop
  run lines
  -- finish up
  GLFW.closeWindow
  GLFW.terminate

-- load a texture
loadGLTextures fileStr = do
  let t = R.readTexture fileStr
  putStrLn $ show t
  {-(Image (Size w h) pd) <- bitmapLoad "Data/NeHe.bmp"-}
  texName <- liftM head (genObjectNames 1)
  textureBinding Texture2D $= Just texName
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  {-texImage2D Nothing NoProxy 0 RGB' (TextureSize2D w h) 0 pd-}
  let f = GL.CompressedTextureFormat gl_COMPRESSED_RGBA_S3TC_DXT5  
  withForeignPtr (T.ptr t) $ \ptr -> do
    let adjustedPtr = ptr `plusPtr` (T.offset t)
    let c = GL.CompressedPixelData f (fromIntegral $ T.size t) ptr
    compressedTexImage2D Nothing NoProxy 0 (TextureSize2D (fromIntegral $ T.width t) (fromIntegral $ T.height t)) 0 c
  return texName

-- we start with waitForPress action
run lines = loop waitForPress
  where 
 
    loop action = do
      -- draw the entire screen
      render lines
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
            windowOpen <- getParam Opened
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
          modifyIORef lines (((x,y):) . ((x,y):))
          return (Action waitForRelease)
 
    waitForRelease = do
        -- keep track of mouse movement while waiting for button 
        -- release
        (GL.Position x y) <- GL.get GLFW.mousePos
        -- update the line with new ending position
        modifyIORef lines (((x,y):) . tail)
        b <- GLFW.getMouseButton GLFW.ButtonLeft
        case b of
          -- when button is released, switch back back to 
          -- waitForPress action
          GLFW.Release -> return (Action waitForPress)
          GLFW.Press   -> return (Action waitForRelease)

render lines = do
  l <- readIORef lines
  GL.clear [GL.ColorBuffer]
  GL.color $ color3 1 0 0
  GL.renderPrimitive GL.Lines $ mapM_
      (\ (x, y) -> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0)) l
 
 
vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3
