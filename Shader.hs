module Shader where

import Global
import Control.Monad (forM)
import Graphics.Rendering.OpenGL.Raw
import Foreign.C.String (withCString, peekCString)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (nullPtr, castPtr)
import Foreign.Storable (peek)
import System.FilePath.Posix (pathSeparator)

data ShaderType = VertexType | GeometryType | FragmentType

shaderFromFile :: ShaderType -> FileName -> IO GLuint
shaderFromFile typ name = do
  str <- readFile $ "Shaders" ++ [pathSeparator] ++ name
  createShader typ str

createShader :: ShaderType -> String -> IO GLuint
createShader sType strFile = do
  putStrLn strFile
  let typ = case sType of 
        VertexType -> gl_VERTEX_SHADER
        GeometryType -> gl_GEOMETRY_SHADER
        FragmentType -> gl_FRAGMENT_SHADER
  shader <- glCreateShader typ
  withCString strFile $ \ptr -> with ptr $ \ptrPtr -> 
    glShaderSource shader 1 (castPtr ptrPtr) nullPtr
  glCompileShader shader
  sLength <- alloca $ \ptr -> do
    glGetShaderiv shader gl_SHADER_SOURCE_LENGTH ptr
    peek ptr
  putStrLn $ show sLength
  status <- alloca $ \ptr -> do
    glGetShaderiv shader gl_COMPILE_STATUS ptr
    peek ptr
  {-putStrLn $ show status-}
  if status /= fromIntegral gl_FALSE then return $ shader
  else do
    let typStr = case sType of 
          VertexType -> "vertex "
          GeometryType -> "geometry "
          FragmentType -> "fragment "
    logLength <- with 5 $ \ptr -> do
      glGetShaderiv shader gl_INFO_LOG_LENGTH ptr
      peek ptr
    putStrLn $ "logLength: " ++ show logLength
    errMsg <- allocaBytes (fromIntegral $ logLength + 1) $ \ptr -> do
      glGetShaderInfoLog shader (fromIntegral logLength) nullPtr ptr
      peekCString (castPtr ptr)
    error $ "Compile failure in " ++ typStr ++ "shader:\n" ++ errMsg

createProgram shaders = do
  prog <- glCreateProgram
  forM shaders $ glAttachShader prog
  glLinkProgram prog
  status <- with 0 $ \ptr -> do
    glGetProgramiv prog gl_LINK_STATUS ptr
    peek ptr
  forM shaders $ glDetachShader prog
  if status /= fromIntegral gl_FALSE then return $ prog
  else do
    logLength <- with 0 $ \ptr -> do
      glGetProgramiv prog gl_INFO_LOG_LENGTH ptr
      peek ptr
    errMsg <- allocaBytes (fromIntegral $ logLength + 1) $ \ptr -> do
      glGetProgramInfoLog prog (fromIntegral logLength) nullPtr ptr
      peekCString (castPtr ptr)
    error $ "Linker failure: " ++ errMsg

deleteShaders shaders = forM shaders $ glDeleteShader
