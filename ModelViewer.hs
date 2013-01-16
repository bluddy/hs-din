module Main where 

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL (get, ($=))
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import System.Environment (getArgs, getProgName)
import Data.Char (toUpper)
import qualified Shader as S
import qualified Data.Array.Storable as St
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Storable (peek, sizeOf)
import Foreign.Ptr (nullPtr, castPtr)
  
initProg = do
  vs <- S.shaderFromFile S.VertexType "basic.vert"
  fs <- S.shaderFromFile S.FragmentType "basic.frag"
  let shaders = vs:[fs]
  prog <- S.createProgram shaders
  S.deleteShaders shaders
  return prog

vertices :: [GLfloat]
vertices = [0.75,  0.75, 0, 1,
            0.75, -0.75, 0, 1,
           -0.75, -0.75, 0, 1]

initVertexBuffer = do
  let a = gl_ARRAY_BUFFER
      l = length vertices
      totalS = l * sizeOf (1 :: GLfloat)
  arr <- St.newListArray (0, l-1) vertices :: IO(St.StorableArray Int GLfloat)
  bufObj <- alloca $ \ptr -> glGenBuffers 1 ptr >> peek ptr
  glBindBuffer a bufObj
  St.withStorableArray arr $ \aptr -> 
    glBufferData a (fromIntegral $ totalS) aptr gl_STATIC_DRAW
  glBindBuffer a 0
  return bufObj

data Action = Action (IO Action)

main = do
  args <- getArgs
  p <- getProgName
  case args of 
    []      -> putStrLn "Please input model path" 
    path:xs -> main' path
 
main' path = do
  GLFW.initialize
  -- open window
  GLFW.openWindowHint GLFW.OpenGLVersionMajor 3
  GLFW.openWindowHint GLFW.OpenGLVersionMinor 2
  GLFW.openWindow (GL.Size 1024 768) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "Model Viewer"
  (a,b,c) <- get GLFW.glVersion
  putStrLn $ show a ++ "." ++ show b ++ "." ++ show c
  version <- do 
    str <- glGetString gl_VERSION
    peekCString (castPtr str)
  putStrLn version
  langVer <- do
    str <- glGetString gl_SHADING_LANGUAGE_VERSION
    peekCString (castPtr str)
  putStrLn langVer
 
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) -> do
    glViewport 0 0 w h

  -- load file hierarchy
  {-filemap <- L.fullFileMap-}
  {-fileStr <- L.readPath filemap path-}

  prog <- initProg
  pbo <- initVertexBuffer
  vao <- alloca $ \ptr -> do 
    glGenVertexArrays 1 ptr
    peek ptr
  glBindVertexArray vao

  -- run the main loop
  run (prog, pbo, vao)
  -- finish up
  GLFW.closeWindow
  GLFW.terminate

-- we start with waitForPress action
run info = loop waitForPress
  where 
    loop action = do
      -- draw the entire screen
      render info
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

render (prog, pbo, vao)  = do
  glClearColor 0 0 0 0
  glClear (fromIntegral gl_COLOR_BUFFER_BIT)

  glUseProgram prog

  glBindBuffer gl_ARRAY_BUFFER pbo
  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 4 gl_FLOAT (fromIntegral gl_FALSE) 0 nullPtr

  glDrawArrays gl_TRIANGLES 0 3

  glDisableVertexAttribArray 0
  glUseProgram 0
 
fileExt filename = map toUpper $ drop (length filename - length "xxx") filename
